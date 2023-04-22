; lisp 通过递归调用，即一个过程通过调用自身来实现运算。这能省略掉循环
; 可这种递归方式，能否完全刻画一个能够有计算机实现的计算过程？这种定义是否稳定？都需要理论依据

; 已经知道了全部的规则，但无法理解每一步背后动作产生的后果
; 需要对计算过程中各种动作的运行情况做出规划，用一个程序去控制这一过程的进展。

; 1.2.1 现行的递归和迭代
;阶乘
; n! = n*(n-1)*....*1 = n*(n-1)!
;递归计算过程：先逐步展开，后逐渐收缩；推迟操作造成的链条增加
;计算复杂度高，但贴近描述
(define (factorial  n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))
(factorial 4)

;product <- counter * product
;counter <- counter + 1
;迭代计算过程：没有任何增长或收缩，用固定数量的状态变量描述的计算过程
;计算复杂度低，但抽象度较高
(define (factorial n)
    (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter ((* counter product) (+ counter 1) max-count))))
  
;exe 1.9 加法的实现
; recursion: 
; (+ 3 4) = (+ 1  (+ 2 4)) = (+ 1  (+ 1 (+ 1 4))) = (+ 1  (+ 1 (+ 1 (+ 0 4)))) 
;         = (+ 1  (+ 1 (+ 1 4))) = ... = 7
(define (+ a b)
    (if (= a 0)
        b
        (+ 1 (- a 1) b))) 

;interation:
;(+ 3 4)=(+ 2 5)=(+ 1 6) = (+ 0 7) =7
(define (+ a b)
    (if (= a 0)
        b
        (+ (- a 1) (+ b 1))))

;exe 1.10 Ackermann function
(define (A x y)
    (cond   ((= y 0) 0)
            ((= x 2) (* 2 y))
            ((= y 1) 2)
            (else (A (- x 1) (A x (- y 1))))
    )))
; (A 1 2);死机
(A 2 4); 8
(A 3 3); 8

; 树形递归 tree recursion
; Fib(n) =    0,                  if n = 0
;             1,                  if n = 1
;             Fib(n-1)+Fib(n-2),  else
(define (fib n)
    (cond   ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1)) 
                     (fib (- n 2))))
    )
)
; time = O(\fe ^n), \fe = (1 + \sqrt(5))/2 ~= 1.6180
(fib 30)
(fib 32)

; 迭代：时间的大头在于过程的抽象，而非执行的时间
; strat :
;     a <- fib(0) = 0
;     b <- fib(1) = 1
; loop:
;     a <- a+b
;     b <- a
; return:
;     a = fib(n+1)
;     b = fib(n)
(define (fib n)
    (fib-iter 1 0 n))
(define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b ) a (- count 1))    
    )
)
(fib 100)

; 换零钱：100块钱，换成25/10/5/1块的，能有多少种方法
; 问题的核心在于将大的问题递归为小的问题（难->简单）
; O(amount, caterger) = ?
; 将总数为 a 的现金换成n种硬币的不同方式数 = 
;     将现金数 a 换成出第一种之外的所有其他硬币的数量 +
;     将现金数 a-d 换成出第一种之外的所有硬币的数量
; 某个给定现金数的换零钱问题 --(递归)--> 更少现金数/更少种类硬币的同类问题 直到问题简单到可以解答
(define (count-change amount)
    (cc amount 5))
(define (cc  amount kinds-of-coins)
    (cond   ((= amount 1 ) 1)
            ((or (< amount 0) (= kinds-of-coins 0)) 0 )
            (else (+ (cc amount (- kinds-of-coins 1))
                     (cc (- amount (first-denominaton kinds-of-coins))
                         kinds-of-coins) 
            ))
    )
)
(define (first-denominaton kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)
    )
)

(count-change 100)
(count-change 500)
; 树形递归非常容易描述和理解，但效率非常低
; 预告：灵巧编译器（自动讲树形递归翻译为一个能计算出同样结果的更有效过程）

; TODO: 迭代实现

; exe1.11
; f(n) = n, if n <3
;        f(n-1)+2f(n-2)+3f(n-3), n>=3
; 递归:
(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3)))
        )
    )
)
(f 0);0
(f 1);1
(f 2);2
(f 3);4
(f 4);11

; 迭代：找到关键的不变计算过程
; f(n+1) = f(n) + 2f(n-1)+ 3f(n-2)
;          f(n) = f(n-1) + 2f(n-2)+ 3f(n-3)
; 不变的是 f(n),f(n-1),f(n-2) = a, b, c
; a' <- a + 2b + 3c 
; b <- a        
; c <- b
; a <- a'
; a, b, c = f(n+1),f(n),f(n-1) 
; f(3) = f(2)+ 2f(1)+3f(0)
; (define (f n)
;     (f-iter 2 1 0 2 n))
; (define (f-iter a b c iter n)
;     (cond ((< n 3) n)
;           ((> iter n) a)
;           (else (f-iter
;                 (+ a (* 2 b) (* 3 c))
;                 a
;                 b
;                 (+ 1 iter)
;                 n
;                 )
;         )
;     )
; )
; (f 4)

