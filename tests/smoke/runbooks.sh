#!/usr/bin/env bash
# SPDX-FileCopyrightText: The gumshoe Authors
# SPDX-License-Identifier: 0BSD
#
# Runs the remediation runbooks against a disposable kind cluster and checks
# the cluster state each one leaves behind: the change happened when it was
# confirmed, and nothing changed when it was refused or declined.
#
#   KUBECONFIG=<kind kubeconfig> tests/smoke/runbooks.sh
#
# It refuses every context whose name does not start with "kind-", because it
# creates, changes and deletes objects. Every object lives in one namespace
# that is deleted at the end.
set -uo pipefail

export KUBERC=/dev/null   # a kuberc may make kubectl delete interactive
context="$(kubectl config current-context 2>/dev/null)"
case "${context}" in
  kind-*) ;;
  *) echo "refusing: context '${context}' is not a kind cluster" >&2; exit 2 ;;
esac

root="$(cd "$(dirname "$0")/../.." && pwd)"
ns="smoke-$(date +%s)"
image="docker.io/library/busybox:1.37"
older="docker.io/library/busybox:1.36"
failures=0

pass() { echo "✅ $*"; }
fail() { echo "❌ $*"; failures=$((failures + 1)); }
expect() {  # expect <description> <wanted> <actual>
  if [ "$2" = "$3" ]; then pass "$1"; else fail "$1 (wanted '$2', got '$3')"; fi
}
jp() { kubectl -n "${ns}" get "$1" -o "jsonpath=$2" 2>/dev/null; }

# book <answer> <runbook> <args...>: runs a runbook, answering its confirmation
# with <answer> on stdin; the exit code is the runbook's.
book() {
  local answer="$1" path="$2"; shift 2
  echo "${answer}" | timeout 600 bb "${root}/${path}" "$@" > "${log}" 2>&1
}
log="$(mktemp)"
trap 'kubectl delete namespace "${ns}" --wait=false > /dev/null 2>&1; rm -f "${log}"' EXIT

kubectl create namespace "${ns}" > /dev/null
kubectl -n "${ns}" create deployment web --image="${image}" --replicas=2 -- sleep 3600 > /dev/null
kubectl -n "${ns}" create deployment idle --image="${image}" --replicas=0 -- sleep 3600 > /dev/null
kubectl -n "${ns}" create deployment scaled --image="${image}" --replicas=1 -- sleep 3600 > /dev/null
kubectl -n "${ns}" autoscale deployment scaled --min=1 --max=3 --cpu=80 > /dev/null 2>&1 \
  || kubectl -n "${ns}" autoscale deployment scaled --min=1 --max=3 --cpu-percent=80 > /dev/null
kubectl -n "${ns}" create cronjob nightly --image="${image}" --schedule="0 3 * * *" -- true > /dev/null
kubectl -n "${ns}" create configmap settings --from-literal=a=b > /dev/null
kubectl -n "${ns}" create configmap kept --from-literal=a=b > /dev/null
kubectl -n "${ns}" rollout status deployment/web --timeout=300s > /dev/null || fail "fixture web did not roll out"

# --- rollout restart ------------------------------------------------------------
book yes runbooks/kubernetes/workloads/restart.clj --pick "Deployment ${ns}/web"
expect "restart exits 0" 0 $?
[ -n "$(jp deployment/web '{.spec.template.metadata.annotations.kubectl\.kubernetes\.io/restartedAt}')" ] \
  && pass "restart stamped the pod template" || fail "restart left no restartedAt"

book yes runbooks/kubernetes/workloads/restart.clj --pick "Deployment ${ns}/idle"
expect "restart refuses a Deployment with no pods" 1 $?
expect "the refused restart changed nothing" "" "$(jp deployment/idle '{.spec.template.metadata.annotations}')"

# --- scale ------------------------------------------------------------------------
book no runbooks/kubernetes/workloads/scale.clj --pick "Deployment ${ns}/web" --replicas 3
expect "a declined scale exits 1" 1 $?
expect "a declined scale changed nothing" 2 "$(jp deployment/web '{.spec.replicas}')"

book yes runbooks/kubernetes/workloads/scale.clj --pick "Deployment ${ns}/web" --replicas 3
expect "scale exits 0" 0 $?
expect "scale reached 3 ready replicas" 3 "$(jp deployment/web '{.status.readyReplicas}')"

book yes runbooks/kubernetes/workloads/scale.clj --pick "Deployment ${ns}/scaled" --replicas 3
expect "scale refuses a Deployment an HPA manages" 1 $?
grep -q "HorizontalPodAutoscaler scaled manages its replicas" "${log}" && pass "the refusal names the HPA" || fail "the refusal does not name the HPA"

# --- rollback -----------------------------------------------------------------------
kubectl -n "${ns}" set image deployment/web "busybox=${older}" > /dev/null
kubectl -n "${ns}" rollout status deployment/web --timeout=300s > /dev/null
expect "fixture: web runs the older image" "${older}" "$(jp deployment/web '{.spec.template.spec.containers[0].image}')"
book yes runbooks/kubernetes/workloads/rollback.clj --pick "Deployment ${ns}/web"
expect "rollback exits 0" 0 $?
expect "rollback restored the earlier image" "${image}" "$(jp deployment/web '{.spec.template.spec.containers[0].image}')"

book yes runbooks/kubernetes/workloads/rollback.clj --pick "Deployment ${ns}/web" --to-revision 99
expect "rollback refuses an unknown revision" 1 $?

# --- CronJob suspend / resume ---------------------------------------------------------
book yes runbooks/kubernetes/cronjobs/suspend.clj --namespace "${ns}" --job nightly
expect "suspend exits 0" 0 $?
expect "suspend set spec.suspend" true "$(jp cronjob/nightly '{.spec.suspend}')"

# With no CronJob left to suspend in the cluster, the book has nothing to do,
# which is a success; with others left it would offer those instead.
book yes runbooks/kubernetes/cronjobs/suspend.clj --namespace "${ns}" --job nightly
code=$?
if grep -q "every CronJob is suspended already" "${log}"; then
  expect "suspending again has nothing to do" 0 "${code}"
else
  expect "suspending again changes nothing" 1 "${code}"
fi
expect "the CronJob stays suspended" true "$(jp cronjob/nightly '{.spec.suspend}')"

book yes runbooks/kubernetes/cronjobs/resume.clj --namespace "${ns}" --job nightly
expect "resume exits 0" 0 $?
expect "resume cleared spec.suspend" false "$(jp cronjob/nightly '{.spec.suspend}')"

# --- delete ------------------------------------------------------------------------------
book no runbooks/kubernetes/objects/delete.clj --pick "configmaps ${ns}/kept"
expect "a declined delete exits 1" 1 $?
expect "a declined delete kept the object" kept "$(jp configmap/kept '{.metadata.name}')"

book yes runbooks/kubernetes/objects/delete.clj --pick "configmaps ${ns}/settings"
expect "delete exits 0" 0 $?
grep -q "the API server accepted a dry run" "${log}" && pass "the confirmation shows the dry run" || fail "the confirmation does not show the dry run"
expect "delete removed the object" "" "$(jp configmap/settings '{.metadata.name}')"

book yes runbooks/kubernetes/objects/delete.clj --pick "configmaps ${ns}/missing"
expect "delete refuses an object the dry run cannot find" 1 $?

echo
if [ "${failures}" -eq 0 ]; then echo "all runbook smoke checks passed"; else echo "${failures} runbook smoke check(s) failed"; fi
exit $(( failures > 0 ))
