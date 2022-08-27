(in-package :cl-raylib)

;; RLAPI Vector2 GetMouseDelta(void);                    // Get mouse delta between frames
(defcfun "GetMouseDelta" (:struct %vector2)
  "Get mouse delta between frames")

(defmacro with-matrix (&body body)
  `(progn (rlgl:push-matrix)
          (unwind-protect (progn ,@body)
            (rlgl:pop-matrix))))

(defun load-image-gced (filename)
  (let* ((res (load-image filename))
         (cpy (raylib::copy-image res)))
    (sb-ext:finalize res (lambda () (unload-image cpy)))
    res))

(defun load-texture-from-image-gced (image)
  (let* ((res (load-texture-from-image image))
         (cpy (raylib::copy-texture res)))
    (sb-ext:finalize res (lambda () (push cpy *textures-to-unload*)))
    res))

(defun some-loading (vec &optional (file-type ".jpg"))
  (cffi::with-pointer-to-vector-data (ptr (if (adjustable-array-p vec)
                                              (copy-seq vec)
                                              vec))
    (load-image-from-memory file-type ptr (length vec))))

(defmacro do-pressed-key ((key) &body body)
  (check-type key symbol)
  `(loop :for ,key := (get-key-pressed)
         :until (eq :key-null ,key)
         :do (progn ,@body)))

(defmacro do-pressed-key-case (&body body)
  `(do-pressed-key (key)
     (case key
       ,@body
       (t
        (format t "unbound key ~S~%" key)
        (force-output)) )))

(declaim (inline scalef))
(defun scalef (&optional (x 1.0) (y 1.0) (z 1.0))
  (rlgl:scale-f (coerce x 'single-float)
                (coerce y 'single-float)
                (coerce z 'single-float)))

(declaim (inline translatef))
(defun translatef (&optional (x 0.0) (y 0.0) (z 0.0))
  (rlgl:translate-f (coerce x 'single-float)
                    (coerce y 'single-float)
                    (coerce z 'single-float)))

(export '(scalef
          translatef
          get-mouse-delta
          load-image-gced
          load-texture-from-image-gced
          some-loading
          do-pressed-key
          do-pressed-key-case
          with-matrix))
