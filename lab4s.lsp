(defun % (a b)
  (= 0 (rem a b)) ; rem(remainder) - остаток от деления = 0 - значит делится, возвращает t или nil в зависимости от исхода
  ; аналогом функции (вообще эта функция используется в ней самой) является truncate который возвращает частное и остаток
  ; т.е. (truncate 10 3) вернёт 3 и 1, частное и остаток
  ; rem же берёт отсюда только остаток
)

(defun div-count (arr c) ; подсчёт делимых c в списке
    (cond
    ((null arr) 0)
    (t (+ (if (% (car arr) c) 1 0) (div-count (cdr arr) c)))
  )
)

(defun check-cycle (listA listB max-element &optional (c 2)) ; цикл проверки списков на соответствие условию
  ; модификатор &optional позволяет аргументу задавать значение по умолчанию
  ; чтобы задать значение по умолчанию, нужно в скобках указать имя аргумента и его значение по умолчанию
  (cond
    ((> c max-element) nil)
    ((> (div-count listA c) (div-count listB c)) c)
    (t (check-cycle listA listB max-element (1+ c)))
  )
)

(defun maxl (l) ; функция нахождения максимального в списке, стандартная функция не умеет работать со списками
  (cond
    ((null l) nil)
    ((= (length l) 1) (car l))
    ((= (length l) 2) (max (car l) (cadr l)))
    (t (max (car l) (maxl (cdr l))))
  )
)

(defun sol (listA listB) ; основная функция, возвращает nil, условие задачи невыполнимо для данных списков, иначе c
  (cond
      ((> (length listA) (length listB)) 1) ; мало ли повезёт
    (t 
      (check-cycle listA listB (maxl (append listA listB)))
    )
  )
)

