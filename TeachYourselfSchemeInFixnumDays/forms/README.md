## Forms代码结构

Scheme 运行一个列表形式的代码结构时，首先要检测列表第一个元素，或列表头。如果这个列表头是一个过程，则代码结构的其余部分则被当成将传递给这个过程的参数集，而这个过程将接收这些参数并运算。

如果这个代码结构的列表头是一个特殊的代码结构，则将会采用一种特殊的方式来运行。我们已经碰到过的特殊的代码结构有`begin`， `define`和 `set!`。



### lambda 的使用

#### 单参

```scheme
> (define add2
  	(lambda (x) (+ x 2)))
> (add2 1)
3
```

#### 多参

```scheme
(define area
  (lambda (length breadth)
    (* length breadth)))
```

而系统过程`*`也可以实现相乘。我们可以简单的这样做：

```scheme
(define area *)
```





`apply`过程允许我们直接传递一个装有参数的list 给一个过程来完成对这个过程的批量操作。

```scheme
(define x '(1 2 3))

(apply + x)
=>  6
```





## 条件语句

### if

最基本的结构就是 if：

```scheme
(if 测试条件
    then-分支
    else-分支)	; else分支是可选的。
```

当我们只需要一个基本条件语句分支时（”then”分支或”else”分支），**使用 when 和 unless会更方便。**

如果`if`的分支有多个代码结构时，需要一个显式的`begin`代码结构。

```scheme
#lang racket
(define p 80)
(if (> p 90)
    (begin 
        (display "then happy")
        (display "happy2")
    )
    (begin 
        (display "else happy")
        (display "happy2")
    ))
```

### cond

`cond`结构在表示多重`if`表达式时很方便，这样的结构都可以使用`cond`来这样写：

```scheme
(cond ((char<? c #\c) -1)
      ((char=? c #\c) 0)
      (else 1))
```

`cond`就是这样的一种多分支条件结构。每个从句都包含一个判断条件和一个相关的操作。第一个判断成立的从句将会引发它相关的操作执行。如果任何一个分支的条件判断都不成立则最后一个`else`分支将会执行(`else`分支语句是可选的)。

cond的分支操作都是`begin`结构。

### case

当`cond`结构的每个测试条件是一个测试条件的分支条件时，可以缩减为一个`case`表达式。

```scheme
(define c #\c)
(case c
  ((#\a) 1)
  ((#\b) 2)
  ((#\c) 3)
  (else 4))
=>  3
```

分支头值是`#\c` 的分支将被执行。

### `and` & `or`

`and`和`or`都是从左向右运算。当某个子结构可以决定最终结果时，`and`和`or`会忽略剩余的子结构，即它们是“短路”的。



## 变量

### let

下面代码表示局部变量`x`被赋值为1，而`y`被赋上了值为20的全局变量`x`。这是由于 `(x 1)`这个初始化的过程并不作为`let`过程结构体的一部分。因此，在初始化时对`x`的引用都指向了全局变量`x`，而不是局部变量`x`。

```scheme
(define x 20)
(let ((x 1) (y x))
    (+ x y))
=> 21
```

### let*

希望用`let`依次创建局部变量，即在初始化区域中用先创建的变量为后创建的变量赋值。`let*`结构就可以这样做：

```scheme
(let* ((x 1)
       (y x))
  (+ x y))
=>  2
```

修改局部函数名

```scheme
(let ((are (lambda (x y) (+ x y)))) (are 2 3))
```



