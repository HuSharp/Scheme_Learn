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

该参数必须向着不断接近结束条件而改变，改变的参数必须在结束条件中得以测试：比如当使用 `cdr` 时，用 `null?` 测试是否结束。





