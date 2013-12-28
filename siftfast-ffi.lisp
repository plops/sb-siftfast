(defpackage :sb-siftfast (:use :cl :sb-alien)
	    (:export
	     #:create-image-from-lisp
	     #:get-keypoints-to-lisp
	     #:read-pgm))
(in-package :sb-siftfast)

(sb-alien:load-shared-object "libsiftfast.so")

;; on my debian 7.3 libsiftfast only works when compiled without openmp:
;; cmake . -DUSE_OPENMP=OFF


;; typedef struct ImageSt {
;;     int rows, cols;          // Dimensions of image.
;;     float *pixels;          // 2D array of image pixels.
;;     int stride;             // how many floats until the next row
;;                             // (used to add padding to make rows aligned to 16 bytes)
;; } *Image;

(define-alien-type nil
    (struct image-struct
	    (rows int)
	    (cols int)
	    (pixels (* float))
	    (stride int)))

(define-alien-type image (* (struct image-struct)))

;; typedef struct KeypointSt {
;;     float row, col;             // Subpixel location of keypoint.
;;     float scale, ori;           // Scale and orientation (range [-PI,PI])
;;     float descrip[128];     // Vector of descriptor values
;;     struct KeypointSt *next;    // Pointer to next keypoint in list.
;; } *Keypoint;

(define-alien-type nil
    (struct keypoint-struct
		     (row float)
		     (col float)
		     (scale float)
		     (ori float)
		     (descrip (array float 128))
		     (next (* (struct keypoint-struct)))
		     (pixels (* float))
		     (stride int)))

(define-alien-type keypoint (* (struct keypoint-struct)))

;; Keypoint GetKeypoints(Image porgimage);
;; Image CreateImage(int rows, int cols);
;; Image CreateImageFromMatlabData(double* pdata, int rows, int cols);
;; void DestroyAllImages();
;; void DestroyAllResources();
;; void FreeKeypoints(Keypoint keypt);


(define-alien-routine ("GetKeypoints" get-keypoints)
    keypoint (im image))

(define-alien-routine ("CreateImage" create-image)
    image (rows int) (cols int))

(define-alien-routine ("CreateImageFromMatlabData" create-image-from-matlab-data)
    image (pdata (* double)) (rows int) (cols int))

(define-alien-routine ("DestroyAllImages" destroy-all-images) void)
(define-alien-routine ("DestroyAllResources" destroy-all-resources) void)
(define-alien-routine ("FreeKeypoints" free-keypoints) void (keypt keypoint))

(defun read-pgm (filename)
  (declare ((or pathname string) filename)
           (values (array (unsigned-byte 8) 2) &optional))
  (with-open-file (s filename)
    (unless (equal (symbol-name (read s)) "P5")
      (error "no PGM file"))
    (let* ((w (read s))
           (h (read s))
           (grays (read s))
           (pos (file-position s))
           (data (make-array (list h w)
                             :element-type '(unsigned-byte 8)))
           (data-1d (make-array (* h w)
                                :element-type '(unsigned-byte 8)
                                :displaced-to data)))
      (declare ((integer 0 65535) grays w h))
      (unless (= grays 255)
        (error "image has wrong bitdepth"))
      (with-open-file (s filename
                         :element-type '(unsigned-byte 8))
        (file-position s pos)
        (read-sequence data-1d s))
      data)))




(defun create-image-from-lisp (a)
  "Convert a 2d unsigned-byte array into an image of single-floats as
expected by libsiftfast."
  (declare (type (simple-array (unsigned-byte 8) 2) a))
  (destructuring-bind (h w) (array-dimensions a)
    (let* ((a1 (make-array (* h w) :element-type '(unsigned-byte 8)
			   :displaced-to a))
	   (f1 (make-array (* h w) :element-type 'double-float)))
      (dotimes (i (* h w))
	(setf (aref f1 i) (* (/ 255d0) (aref a1 i))))
      ;; the following function will convert the double floats into
      ;; single float
      (create-image-from-matlab-data (sb-sys:vector-sap f1)
				     h w))))



(defun keypt-length (keypts)
 (let ((k keypts)
       (i 0))
   (loop while (not (null-alien k))
      do (setf k (slot k 'next))
      (incf i))
   i))

(defun get-keypoints-to-lisp (im)
  "Convert a 2d array of unsigned-byte into float and return the sift
coefficients as a lisp array."
  (let* ((fim (create-image-from-lisp im))
	 (keypts (get-keypoints fim))
	 (n (keypt-length keypts))
	 (a (make-array (list n 128) :element-type 'single-float))
	 ;; row col scale ori
	 (b (make-array (list n 4) :element-type 'single-float)))
    (let ((k keypts) 
	  (i 0))
      (loop while (not (null-alien k)) do
	   (let ((desc (slot k 'descrip)))
	     (dotimes (j 128)
	       (setf (aref a i j) (sb-alien:deref desc j)))
	     (setf (aref b i 0) (slot k 'row)
		   (aref b i 1) (slot k 'col)
		   (aref b i 2) (slot k 'scale)
		   (aref b i 3) (slot k 'ori)))
	   (setf k (slot k 'next))
	   (incf i)))
    (prog1
	(values b a)
      (free-keypoints keypts)
      (destroy-all-resources))))

#+nil
(progn
  (defparameter *im* (read-pgm "test2.pgm"))
  (defparameter *fim* (multiple-value-list (get-keypoints-to-lisp *im*))))

