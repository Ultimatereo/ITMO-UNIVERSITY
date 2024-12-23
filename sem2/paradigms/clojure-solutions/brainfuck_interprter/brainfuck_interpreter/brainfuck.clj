(ns brainfuck-interpreter.brainfuck)

(defn initializeState [program input memoryLimit stepLimit]
  {:program (vec program)
   :input (seq input)
   :memory (vec (repeat memoryLimit 0))
   :pc 0
   :mp 0
   :steps 0
   :stepLimit stepLimit
   :output []})

(defn executeInstruction [state]
  (let [{:keys [program input memory pc mp]} state
        instruction (program pc)
        currentCell (memory mp)]
    (cond
      (= instruction \>) (assoc state :mp (inc mp) :pc (inc pc))
      (= instruction \<) (assoc state :mp (dec mp) :pc (inc pc))
      (= instruction \+) (assoc state :memory (assoc memory mp (mod (inc currentCell) 256)) :pc (inc pc))
      (= instruction \-) (assoc state :memory (assoc memory mp (mod (dec currentCell) 256)) :pc (inc pc))
      (= instruction \.) (assoc state :output (conj (:output state) (char currentCell)) :pc (inc pc))
      (= instruction \,) (assoc state :memory (assoc memory mp (if input (int (if (first input) (first input) 0)) 0)) :input (if input (rest input) input) :pc (inc pc))
      (= instruction \[) (if (zero? currentCell)
                           (assoc state :pc (->> (iterate inc (inc pc))
                                                 (drop-while #(and (< % (count program)) (not= (program %) \])))
                                                 (first)))
                           (assoc state :pc (inc pc)))
      (= instruction \]) (if (not (zero? currentCell))
                           (assoc state :pc (->> (iterate dec (dec pc))
                                                 (drop-while #(and (>= % 0) (not= (program %) \[)))
                                                 (first)))
                           (assoc state :pc (inc pc)))
      :else (throw (Exception. (str "Unknown command: " instruction " at position " pc))))))


(defn runProgram [program input & {:keys [memoryLimit stepLimit]}]
  (let [memoryLimit (or memoryLimit 30000)
        stepLimit (or stepLimit 100000)
        state (initializeState program input memoryLimit stepLimit)]
    (loop [state state]
      (let [{:keys [pc program steps stepLimit]} state]
        (cond
          (>= steps stepLimit) (println "Error: Step limit exceeded")
          (>= pc (count program)) (do
                                    (println "Program output:" (apply str (:output state)))
                                    (println "Execution completed"))
          :else (recur (executeInstruction (assoc state :steps (inc steps)))))))))

;; Пример использования:
; Выводит то, что было передано:
(runProgram ",[.,]" "Hello, World!")
; Вывод со смещением всех символов на один вперёд
(runProgram ",[+.,]" "Hello, World!")
; Вывод со смещением всех символов на один назад
(runProgram ",[-.,]" "Hello, World!")
; Самая простая программа с HelloWorld
(runProgram ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.>>>++++++++[<++++>-<.>>>++++++++++[<+++++++++>-]<---.<<<<.+++.------.--------.>>+.>++++++++++." "")
; Ограничения по памяти и по операциям
(runProgram ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.>>>++++++++[<++++>-<.>>>++++++++++[<+++++++++>-]<---.<<<<.+++.------.--------.>>+.>++++++++++." "" :stepLimit 10)
(runProgram ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.>>>++++++++[<++++>-<.>>>++++++++++[<+++++++++>-]<---.<<<<.+++.------.--------.>>+.>++++++++++." "" :stepLimit 10 :memoryLimit 2)
