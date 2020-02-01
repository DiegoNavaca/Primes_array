; Basic function to check if a number is prime
(defun is_prime (num)
  (let ((squrt (sqrt num)) (it 1))
    (loop
      (when (= (mod num (aref prime_array it)) 0) (return nil))   ; If it's divisible, then it's not prime
      (when (> (aref prime_array it) squrt)  (return t))          ; If the loop ends, then it's prime
      (incf it)
    )
  )
)

; Function to make an array of prime numbers
; @param limit_by_value:  if t, calculate all prime numbers from 2 to limit
;                         if nil, calculate the first (limit) prime numbers
(defun primes_into_array (limit limit_by_value)
  (setf prime_array (make-array limit :fill-pointer 0))
  (let ((count 0) (it 5))
    (vector-push 2 prime_array)              ; For the function to work properly we have to initialice the first prime
    (incf count)
    (when (> limit 1)
    (vector-push 3 prime_array)              ; We want to check only odd numbers so we have to add 3 to the list
    (incf count)
      (when (> limit 2)
        (loop
          (when (is_prime it)                 ; If it's prime we add it to the list
            (vector-push it prime_array)
            (incf count)
          )
          (incf it 2)                         ; We skip even numbers
          (if limit_by_value
            (when (>= it limit) (return prime_array))
            (when (= count limit) (return prime_array))    ; When we reach the number of primes whe want we stop
          )
        )
      )
    )
  )
)

; Write the array into a .txt file
; @param name: name of the file you want to write
(defun write_file (&optional (name "primes"))
  (let ((len (length prime_array)))
    (with-open-file (str (concatenate 'string "./" name ".txt" )    ; Open or create the file in which to write the array
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "~d~%" len)                                       ; Write the length of the array first
      (loop for i from 0 to (- len 1)
        do
        (format str "~d~%" (aref prime_array i))                    ; Write a number in each line
      )
    )
  )
)

; Load the array from a .txt file
; @param filename: file to load
(defun load_file (&optional (filename "./primes.txt"))
  (let ((in (open filename :if-does-not-exist nil)))
   (when in
     (setf prime_array (make-array (parse-integer (read-line in nil)) :fill-pointer 0))  ; Create or redefine the  with the capacity read from the file
     (loop for line = (read-line in nil)

      while line do (vector-push (parse-integer line) prime_array))                      ; Fill the array
      (close in)
   )
  )

)

; A few function calls to test that everything works fine
(primes_into_array 100 nil)
(write_file)
(load_file)
(write prime_array)
