<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-upload

The upload tool package for [gumshoe](../../README.md): the nginx + php-fpm
upload chain behind a Moodle/Nextcloud app. A standalone detective book - it
registers no scope or capability, so depending on it simply puts the book on
the classpath (no env.edn :plugins entry needed).

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/upload"}}}
```
