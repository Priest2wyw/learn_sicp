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
(define (f n)
    (f-iter 2 1 0 3 n))
(define (f-iter a b c iter n)
    (cond ((< n 3) n)
          ((> iter n) a)
          (else (f-iter
                (+ a (* 2 b) (* 3 c))
                a
                b
                (+ 1 iter)
                n
                )
        )
    )
)
(f 3);4
(f 4);

; ex1.12 帕斯卡三角形
; Write a procedure that computes elements of Pascal’s triangle 
; by means of a recursive process
; pascal(row, col)=
;     1 ,if col = 1 or col = row
;     pascal(row-1, col)+pascal(row, col-1), else
(define (pascal row col)
    (if (or (= col 1) (= col row))
        1
        (+ (pascal (- row 1) col)
           (pascal row (- col 1)))
    )
)

(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)

; ex1.13 fib 
; https://sicp.readthedocs.io/en/latest/chp1/13.html

; ex1.14 
; graph:https://sicp.readthedocs.io/en/latest/chp1/14.html
; time = O(2^n)
; space = O(n)?

; ex1.15 sin
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
    (if (< angle 0.1)
        angle
        (p (/ angle 3)))
)

; (a) the number of using of p in sine(12.15)
; 12.15 > 0.1 ->
; 4.05  > 0.1 ->
; 1.35  > 0.1 ->
; 0.45  > 0.1 ->
; 0.15  > 0.1 ->
; 0.05 (end) 
; p used 5 times

; (b) the use of time and space when computer (sine a)
; p 的调用次数为ceiling(log_3(10a)); ceiling为向上取整
; log_3(10a)  = log_3(10)+log_3(a) 
;             = O(log_3(a)) = O(log(a)/log(3)) 
;             = O(log(a))
; time = O(log(a))
; space = O(log(a))

; 1.2.4 求幂
; 递归定义:
;     b^n = b*b^(n-1)
;     b^0 = 1
(define (expt-rec b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1))))))
(trace-entry expt-rec)
(expt-rec 2 10);线性递归过程,time=O(n),space=O(n)


; 迭代定义:(b counter product)
(define (expt-it b n)
    (expt-iter b n 1)
)
(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
            (- counter 1)
            (* b product)
        )
    )
)
(trace-entry expt-iter)
(expt-it 2 10);线性递归过程,time=O(n),space=O(1)

; 继续优化，time = O(log(n))
; b^8 = (b*(b*(b*(b*(b*(b*(b*b))))))
; >>> 优化为 3 次计算
; >>> b^2 = b*b
; >>> b^4 = b^2*b^2
; >>> b^8 = b^4*b^4
; b^n =   (b^(n/2))^2, n为偶数
;         b*b^(n-1)  , n为奇数
(define (fast-expt b n)
    (cond 
        ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
    )
)
(define (even? n)
    (= (remainder n 2) 0)
)
(trace-entry fast-expt)
(fast-expt 2 10)
; time=space=O(log(n))

; ex1.16 fast-expt-iter
(define (fast-expt-it b n)
    (expt-iter b n 1)
)
;; -------- the next program is error ---------
;; 抽象计算过程不一致，程序的结构也会发生变化，即使是一样的任务
; (define (fast-expt-iter b counter product)
;     (cond 
;         ((= counter 0) 1)
;         ((even? counter) (fast-expt-iter b (/ counter 2) (square product)))
;         (else (fast-expt-iter b  (- counter 1) (* b product)))
;     )
; )
; (trace-entry fast-expt-iter)
; (fast-expt-it 2 10)

;------ iter trace --------
; (expt 2 10 1)
; (expt 4 5 1)
; (expt 4 4 4)
; (expt 16 2 4)
; (expt 256 1 4)
; (expt 256 0 4) 
;  1024
(define (fast-expt-iter b n a)
    (cond 
        ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b  (- n 1) (* b a)))
    )
)
(fast-expt-it 2 10)

; ex1.17 对数复杂度的加法
(define (double n)
    (+ n n ))
(define (halve n)
    (\ n 2))

(define (multi a b)
    (cond ((= b 0)
            0)
          ((even? b)
            (double (multi a (halve b))))
          (else
            (+ a (multi a (- b 1))))))
(multi 2 4)

; ex1.18 
(define (multi a b)
    (multi-iter a b 0))

(define (multi-iter a b product)
    (cond ((= b 0)
            product)
          ((even? b)
            (multi-iter (double a)
                        (halve b)
                        product))
          ((odd? b)
            (multi-iter a
                        (- b 1)
                        (+ a product))
        )
    )
)

; ex1.19
; T-transformer(a1,b1)—>(a2, b2):
;     a2 = b1q+a1q+a1p
;     b2 = b1p + a1q
; T^2(a1, b1) ->(a3, b3)
;     a3 = b1(q^2+2pq)+a1(2q^2+2pq+p^2)
;        = b1(q^2+2pq)+a1(q^2+2pq)+a(q^2+p^2)
;     b3 = b1(p^2+q^2)+a1(q^2+2pq)
;     let 
;         p' = p^2+q^2
;         q' = q^2+2pq
;     then 
;         a3 = b1q' + a1q' + a1p'
;         b3 = b1p' + a1q'
(define (fast-fib n)
    (fast-fib-iter 1 0 0 1 n)
)
(define (fast-fib-iter a b p q count)
    (cond 
        ((= count 0) b)
        ((even? count) 
         (fast-fib-iter a 
                        b 
                        (+ (square p) (square q))
                        (+ (square q) (* 2 p q))
                        (/ count 2)
         )
        )
        (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)  
              )
        )
    )
)
(fast-fib 2000)

; gcd, greatest common divisor
; Euclid-method
; GCD(a, b) = GCD(b, r)
; r = (remainder a b)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)
(trace-entry gcd)
(gcd 122312312 121233);time=space=O(log(n))
(trace-entry remainder)
(gcd 206 40)

; ex1.20
; 正则序求值：完全展开而后归约
; (gcd 206 40)
; = (gcd 40 (remainder 206 40))
; = (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; = (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40)  (remainder 40 (remainder 206 40)) (remainder 40 (remainder 206 40))))
; = ....

; 应用序求值：先求值参数而后应用
; (gcd 206 40)
; = (gcd 40 (remainder 206 40)) = (gcd 40 6)
; = (gcd 6 (remainder 40 6)) = (gcd 6 4)
; = (gcd 4 (remainder 6 4)) = (gcd 4 2)
; = (gcd 2 (remainder 4 2)) = (gcd 2 0)
; = 2

; 1.2.6 素数检测
; 寻找最小因子：
;     1. test-divisor = 2
;     2. 判断 test-divisor < n:
;         真则进行下一步，
;         假则返回 n
;     3. 判断 test-divisor 能否整除 n:
;         真则返回 test-divisor
;         假则进行下一步
;     4. test-divisor = test-divisor + 1, 并重复上述步骤
 ; 后续将第2步重n改进为square_root n,基于如下事实:
;     若d为n的因子，则n/d也是，d和n/d都小于sqrt(n)
(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
    )
)
(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

; fermat-test:  judge is prime in O(log(n))
; if n is prime, for any 0<a<n, has
;         mod(a^n, n) = mod(a, n)

(define (expmod base exp m)
    (cond ((= exp 0) 1)
        (   (even? exp) 
            (remainder (square (expmod base (/ exp 2) m )) 
                m
            )
        )(
            else
            (remainder (* base (expmod base (- exp 1) m))
                m
            )
        )
    )
)
(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
        )
    (try-it (+ 1 (random (- n 1))))
)
(define (fast-prime? n times)
    (cond 
        ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else  false)
    )
)

; ex1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999);7

; ex1.22 runtime
; (define (timed-prime-test n)
;     (newline))
