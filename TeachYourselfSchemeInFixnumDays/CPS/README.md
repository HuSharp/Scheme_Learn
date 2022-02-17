## continuation`和`call/cc

### call-with-current-continuation

[什么是「continuation」?](https://www.zhihu.com/question/61222322)

`call-with-current-continuation`用`current-continuation`来调用它的参数（一个只有一个参数的过程）【在调用时传入参数`current-continuation`，译者注】。这就是这个操作符名字的解释了。但是由于这个名字太长，故通常缩写为`call/cc`[1]。

```scheme
(... (call/cc (lambda (cc) 
                      (<body>))) ...)
```

continuation 也是一个 procedure ，也是 first-class 对象，因此还可以被当成变量传来传去，比如赋值给某个全局变量，那么就等于在表达式外也捕获了这个特定位置的 continuation ；

- 如果在`<body>`内部调用执行了cc，那么整个表达式的值就是这个cc的结果，无关 `<body>` 内的其他操作，整个表达式就可以**直接返回这个结果**，因此cc的调用可以用以模拟非本地退出。
- 如果在 `<body>` 内没有发生cc的调用，那么 `call/cc` 的调用结果就是 `<body>` 的计算结果，再放入整个表达式计算得到表达式的最终结果。

譬如对于下面的程序

```scheme
(define r #f)
(+ 1 (call/cc
        (lambda (k) 
            (set! r k)
            (+ 2 (k 3)))))

(r 5)

; output
4
6
```

- 首先在执行到 `k 3` 时，此时 k 值被赋值为 3，并进行返回运行 `(+ 1 3)` 因此输出 4。
- 在执行`(r 5)` 时继续到 `(+ 2 3)` ，因此 5 为 `continuation` 的返回结果，再运行 `(+ 1 5)` 得到 6。



### 【应用】“退出”续延

`call/cc` 在退出函数或循环时非常有用

对于下面的这个过程：

```scheme
; 接收一个数字列表并把所有的数乘起来
(define list-product
    (lambda (s) 
        (let recur
            ((s s))
            (cond 
                ((null? s) 1)
                (else (* (car s) (recur (cdr s))))))))

(list-product '(6 1 2 3 4 5 6))
(list-product '(6 1 2 3 0 4 5 6))
```

这个方法有一个问题。如果列表中有一个数是0，而且0后面还有很多元素，那么结果是可以预知的。如果这样上面的代码会在得出结果前产生很多无意义的递归调用。这就是“退出”续延大显身手的时候。用`call/cc`，我们可以这样重写这个过程：

```scheme
(define list-product-exit
    (lambda (s) 
        (call/cc
            (lambda (exit) 
                (let recur
                    ((s s)) 
                (cond 
                    ((null? s) 1)
                    (else (cond 
                        ((= (car s) 0) (exit 0))
                        ((* (car s) (recur (cdr s))) )))))))))

(list-product-exit '(6 1 0 2 3 4 5 6))
```

如果遇到一个为0的元素，续延`exit`就会以参数0被调用，这样就防止了更多的调用`recur`。