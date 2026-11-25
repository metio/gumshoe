;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.files
  "Local files that hold secrets, created owner-only from the start: a
   directory rwx------ and files rw-------, each created with its
   permissions in the same call, so no other user can open it in between,
   whatever the umask. Neither replaces what already exists, so a
   planted file or symlink is refused instead of written through."
  (:import (java.nio.file Files LinkOption Path StandardOpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(defn- permissions
  [spec]
  (into-array FileAttribute [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString spec))]))

(defn- path
  ^Path [p]
  (.toAbsolutePath (.toPath (java.io.File. (str p)))))

(defn create-private-directory!
  "Creates directory rwx------, and every missing parent the same way. The
   directory itself must not exist yet: an existing one may have been made
   by somebody else, or with wider permissions. Returns its path as a
   string."
  [directory]
  (let [target (path directory)
        parent (.getParent target)]
    (when (and parent (not (Files/isDirectory parent (make-array LinkOption 0))))
      (Files/createDirectories parent (permissions "rwx------")))
    (str (Files/createDirectory target (permissions "rwx------")))))

(defn write-private!
  "Writes content to a new file rw-------. The file must not exist yet.
   Returns its path as a string."
  [file content]
  (let [target (path file)
        options (java.util.HashSet. [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE])]
    (with-open [channel (Files/newByteChannel target ^java.util.Set options (permissions "rw-------"))]
      (let [buffer (java.nio.ByteBuffer/wrap (.getBytes (str content) "UTF-8"))]
        (while (.hasRemaining buffer)
          (.write channel buffer))))
    (str target)))

(defn mode
  "The POSIX permissions of path as a string such as \"rw-------\"."
  [p]
  (PosixFilePermissions/toString (Files/getPosixFilePermissions (path p) (make-array LinkOption 0))))
