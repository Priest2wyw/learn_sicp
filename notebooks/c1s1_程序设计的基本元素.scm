486
;程序设计的基本元素
;;前缀表示：基础的计算：组合式
(+ 137 349)
(- 100 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)
;前缀表示的优点
;1. 完全适应可能带有任意个实参的过程
(+ 21 35 12 7)
(* 25 4 12)
;2. 可以直接扩充，允许出现组合式嵌套的情况
(+ (* 3 5) (- 10 6))
; 美观打印
(+ ( * 3 
        (+(* 2 4)
        (+ 3 5)))
    ( + (- 10 7)
        6))
; 读入-求值-打印循环


; 命名与环境
; 变量：名字标识符，通过名字取实用计算过程的方式
(define size 2)
size
(* 5 size)
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
((define circumference (* 2 pi radius))
circumference
; 环境：lisp 没有标准的数据结构，也就意味着任何程序都可以视为内部的数据结构
; 环境：用于存储值与符号关联的存储系统

; 组合式的求值
; 本章的一个目标，就是把与过程性思维有关的各种问题隔离出来

; 求解一个表达式的过程：
; 1)求值该表达式的哥哥子表达式
; 2）将作为最左子表达式（运算符）的值的过程应用于相应的实际的实际参数
; 递归与树形表示法


; 1.4 复合过程
; 过程定义（define of process）：
(define (square x) (* x x))

(square 21)
(square (+ 2 5))
(square (square 3))
; 将过程嵌套到过程之中，用于定义复杂过程
(define (sum-of-square x y)
    (+ (square x)(square y )))
(sum-of-square 3 4)

(define (f a)
    (sum-of-square (+ a 1) (* a 2))
)
(f 5)

; 1.1.5 过程应用的代换模型
; 假定：基本运算符应用于实参的机制已经在解释器里做好了。
; 复合过程的计算过程：
; 将复合过程应用于实际参数，就是在将过程体中的每个形参用实际的实参取代，对这一过程体求值。
# 正则序求值
; (f 5)
; # 正则序求值：完全展开而后归约
; ; (sum-of-square (+ 5 1) (* 5 2))
; ; (+     (square (+ 5 1)) (square (* 5 2)))
; ; (+     (* (+ 5 1) (+ 5 1))(* (* 5 2) (* 5 2)))
; ; 归约如下：
; ; (+  (* 6 6)                (* 10 10))
; ; (+      36                     100)
; ;                 136
; # 应用序求值：先求值参数而后应用
; ; (sum-of-square (+ 5 1) (* 5 2)) 
; ; = (+ (square (+ 5 1)) (square (* 5 2)))
; ; ||(square 6) = 36 (square 10) = 100
; ; ||(+ 5 1) = 6 (* 5 2 ) = 10
; ; = (+ 36 100)
; ; =136
; 正则序 V.S. 应用序（lisp采用方案）
; 1. 应用序能避免对表达式的反复求值
; 2. 在超出了可以采用替换方式模拟的过程后，正则序处理会变得更复杂



; 1.1.6 条件表达式和谓词
; 分情况处理
; 谓语:返回真或假的过程、能求得真或假值的表达式
(define ( abs x)        
    (cond ((> x 0) x)
        ((= x 0 ) 0 )
        ((< x 0) (- x))))

(define (abs x)
    (cond ((< x 0) (- x))
        (else x)))
(define (abs x)
    (if (< x 0) 
        (- x)
        x)) 
(abs 4)
(abs (- 4))

; # 连接词
; (and <e1> ... <en>)
; (or <e1> ... <en>)
; (not <e>)
; (and (> x 5) (< x 10))
(define (>= x y)
    (or (> x y) (= x y)))
(define (>= x y)
    (not (< x y)))
(>= 1 2)
(>= 2 1)

; # ex1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a  3)
(define b (+ a 1))
(+ a b  (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
    ((< a b) b)
    (else -1))
(+ a 1))

; #ex1.2
(/  (+ 5 4
        (-  2 
            (- 3
                (+ 6 (/ 4 5))))) 
    (*  3
        (- 6 2)
        (- 2 7)))

; #ex1.3
(define (max a b c)
    (cond   ((and (>= a b)  (>= a c)) a) 
            ((and (>= b a)  (>= b c)) b)
            (else                     c)))
(max 1 2 3);3
(max 2 1 3);3
(max 3 2 1);3

; # 1.1.7 牛顿法求平方根
; 数学函数与计算过程之间存在一个重大的差异：计算过程必须是有效可行的
; sqrt def of math:  
;     y = sqrt(x) if and only if y>0 and x = y^2
; (define (sqrt x)
;     (y (and (> y 0)
;             (= (square y) x))))     
; 数学概念可以用于判断、推导，但无法给出平方根的具体数值
; 函数：描述一件事物特征（是什么）
; 过程：描述如何做这件事（怎么做）


(define (sqrt-iter guess x)
    (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) 
                x)))
(define (improve guess x)
    (average guess (/ x guess)))
(define (average x y)
    (/ (+ x y) 2))  
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x)) 
(sqrt 9)
(sqrt 100)
(sqrt (+ 100 7))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 100) )

# ex1.6
(define (new_if predicate then-clause else-clause)
    (cond (predicate then-clause)
        (else else-clause)))
(new_if (= 2 3) 0 5)

(define (sqrt-iter guess x)
    (new_if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) 
                x)))
;Aborting!: maximum recursion depth exceeded

; #ex1.7
(square (sqrt 0.00000001));e-8


(define (good-enough? guess x)
    (< (rate_diff guess (improve guess x) ) 0.001))
(define (rate_diff guess next_guess)
    (abs (- 1 
            (/ guess next_guess))))

; #ex1.8 立方根 \frac{\frac{x}{y^2}+2y}{3}
(define (cubic_iter  guess x)
    (if (good_enough_cube guess x)
        guess
        (cubic_iter 
            (improve_cube guess x)
            x)))
(define (improve_cube guess x)
    (/  (+ (/ x (* guess guess))
           (* 2 guess))
        3))
(define (good_enough_cube guess x)
    (< (rate_diff guess (improve_cube guess x) ) 0.001))
; (define (good_enough_cube guess x)
;     (< (abs 
;             (- (cube guess)
;                 x)) 
;             0.001))
(define (cube x)
    (* x x x)))
(define (cube_root x)
    (cubic_iter 1.0 x))
(define (cube_root_faker x)
    (cubic_iter 1 x))    
; 试比较以下两种结果
(cube_root 8)
(cube_root_faker 8)

; 1.1.8 过程作为黑箱抽象
; 约束变量与自由变量、作用域、命名冲突、块结构

; 出于防止函数名冲突的考虑，将整个计算过程中用到的函数，转为域内定义，这便是块结构
(define (sqrt x)
    (define (sqrt-iter guess x)
        (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) 
                    x)))
    (define (improve guess x)
        (average guess (/ x guess)))
    (define (average x y)
        (/ (+ x y) 2))  
    (define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))
    (sqrt-iter 1.0 x)) 

; 完全可以将变量名转移到作用域之中
(define (sqrt x)
    (define (sqrt-iter guess )
        (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess ))))
    (define (improve guess )
        (average guess (/ x guess)))
    (define (good-enough? guess )
        (< (abs (- (square guess) x)) 0.001))
    (sqrt-iter 1.0)) 
(sqrt 9)