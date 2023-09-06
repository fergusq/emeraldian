(ns emeraldian.file-reader)

(defmacro read-file [path]
  (slurp path))