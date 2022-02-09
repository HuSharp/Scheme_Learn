# 五法十诫

[代码实现](https://github.com/pkrumins/the-little-schemer)

## 五法

![image-20220206202308553](../assets/blog_image/README/image-20220206202308553.png)

### 一、`car` 仅针对非空列表

```scheme
> (cdr '(1))
'()
> (car '(1))
1
```

### 二、cdr 仅针对非空列表

cdr 的发音是 “could-er"， 且 cdr 的结果为另一个列表

### 三、cons 第二个参数是任意**列表**，且结果为一个列表

cons 有两个参数，第一个参数是任意的 S-表达式，第二个参数是任意**列表**。

```scheme
> (cons '(1 2 3) '(4 5 6))
'((1 2 3) 4 5 6)
> (cons '(1 2 3) '())
'((1 2 3))
```

### 四、Null? 仅针对列表

```scheme
> (null? '())
#t
```

### 五、eq? 需要两个参数，每个参数都必须是一个原子

首先实现一个判断 `atom` 的函数

```scheme
#lang racket
(define atom?
    (lambda (x) 
        (and (not (pair? x)) (not (null? x)))))

(atom? "aa")
```



## 十诫

![image-20220206212850612](../assets/blog_image/README/image-20220206212850612.png)

![image-20220206212859617](../assets/blog_image/README/image-20220206212859617.png)

### 一、在表达任意函数时，总是将询问 null? 作为问题之首

- 在对一个原子列表 lat 进行递归调用时，询问 `(null?lat) 和 else`
- 在对一个数字 n 进行递归调用时，询问 `(zero?n) 和 else`
- 在对一个 S-表达式列表 l 进行递归调用时，询问三个有关 l 的问题：`(null? lat)、(atom? (car l)) 和 else`

#### lat?

```scheme
(define lat?
    (lambda (l) 
        (cond 
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

(lat? '(1 2 3))
```

#### member?

```scheme
; 判断 a 是否为 lat 的成员
(define member?
    (lambda (a lat) 
        (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat))
                            (member? a (cdr lat)))))))

(member? 1 '(1 2 3))
(member? "a" '("a" "b" "c"))
```

#### rember

```scheme
; 从 lat 中移除 a
(define rember
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? (car lat) a) (cdr lat))
                    (else (rember a (cdr lat))))))))

(rember 1 '(1 2 3))
(rember "a" '("a" "b" "c"))
```

#### rember*

现在不仅要递归列表的 cdr， 还要递归列表的 car。

```scheme
; The rember* function removes all matching atoms from an s-expression
;
(define rember*
    (lambda (a lat) 
        (cond 
            ((null? lat) '())
            ((atom? (car lat))
                (cond 
                    ((eq? (car lat) a) (rember* a (cdr lat)))
                    (else (cons (car lat)
                                (rember* a (cdr lat))) )))
            (else (cons (rember* a (car lat))           ; (car lat) 可以不为原子
                        (rember* a (cdr lat)) ) ) )))
```

### 二、使用 cons 来构建列表

#### rember2

```scheme
; cons 构建的第二版本 rember
(define rember2
    (lambda (a lat) 
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? (car lat) a) (cdr lat))
                    (else (cons (car lat) 
                                (rember2 a (cdr lat)))))))))
```

### 三、构建一个列表时，描述第一个典型元素，之后 cons 该元素到一般性递归中。

对于 `firsts` 这样一个函数

```scheme
>(firsts '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant))) 
'(apple plum grape bean)
```

如上面可知，取列表第一个元素。

```scheme
; cons 构建的第二版本 rember
(define firsts
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (car (car l))
                        (firsts (cdr l)))))))
```

可以看到实现中的最后一行便正是第三诫的归纳。

[[What is the definition of "natural recursion"?](https://stackoverflow.com/questions/32260444/what-is-the-definition-of-natural-recursion)]

![image-20220206231609414](../assets/blog_image/README/image-20220206231609414.png)

接着进行种种练习，包括：

`insertR` 、`insertL`、`subst`、`subst2`、`multirember`、`multiInsert`...



### 四、在递归时总是改变至少一个参数。

递归时总是改变至少一个参数。

- 当对一个原子列表 lat 进行递归调用时，使用 （cdr lat）；
- 当对数字 n 进行递归调用时，使用 （sub1 n)；
- 当对一个 S-表达式 lat 进行递归调用时，只要是 (null? lat) 和 (atom? (car lat)) 都不为 true，那么同时使用 (car lat) 和 (cdr lat)；

该参数必须向着不断接近结束条件而改变，改变的参数必须在结束条件中得以测试：比如当使用 `cdr` 时，用 `null?` 测试是否结束；当使用 `sub1` 时，用 `zero?` 测试是否结束。



## 数字的递归转换

首先介绍两个基础函数 `add1` 和 `sub1`。

```scheme
(define add1
    (lambda (n)
        (+ n 1)))
        
(define sub1
    (lambda (n)
        (- n 1)))
```

此处的 `(cond ((zero? m) n)...)  `  就类似一诫中的 `null?` 判断数字是否为空。

此处的 `(o+ m (sub1 m))` 就类似 `cons` 用于构建列表一样构建数字。

```scheme
; The o+ function adds two numbers
;
(define o+
    (lambda (m n)
        (cond 
            ((zero? m) n)       
            (else (add1 (o+ m (sub1 m)))))))
```

o- 以两个数字作为参数，并递减第二个参数 n 直到 0，n 减到 0 需要多少次，m 也就减去多少个 1，最终 m 作为结果返回。（o+ 同理来理解）

```scheme
(define o-
    (lambda (m n)
        (cond 
            ((zero? n) m)
            (else (sub1 (o- m (sub1 n))) ))))
```



- 数字定义：其要么为 0，要么为 1 加到剩余部分，而剩余部分又是一个数字。

- 针对数字的一般性递归是怎样的？		`(sub1 n)`

- 针对数字的一般性结束条件是什么？      `(zero? n)`


接着进行种种练习，包括：

- 运算符的实现：`o+` 、` o-`、`addtup`、`o*`、`tup+`、`o>`、`o<` 、` o=`、`o^`、`o/`、

- 练习题：`olength`、`pick`、`rempick`、`no-nums`、`all-nums`、`eqan?`、`occur`...



### 五、结束代码行的取舍

- 当采用 o+ 构建时，总是使用 0 作为结束代码行的值
- 当采用 o* 构建时，总是使用 1 作为结束代码行的值
- 当采用 cons 构建时，总是使用 `()` 作为结束代码行的值





### 六、简化工作只在功能正确后开展

详情见 `TheLittleScheme/chap5_stars/star*.rkt` 文件中 `eqlist` 和 `equal` 的简化过程。 

