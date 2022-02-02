### 数据类型



Scheme中的简单包含 `booleans` (布尔类型) , `number`(数字类型), `characters`(字符类型) 和 `symbols`(标识符类型)。



在一个需要boolean类型的上下文中，Scheme会将任何非 `#f`的值看成true。

```scheme
(not #f)              =>  #t
(not #t)              =>  #f
(not "Hello, World!") =>  #f
```



字符类型数据有自己的比较判断过程：`char=?`, `char<?`, `char<=?`, `char>?`, `char>=?`



要实现忽略大小写的比较，得使用`char-ci` 过程代替`char`过程：

```scheme
(char-ci=? #\a #\A) =>  #t
(char-ci<? #\a #\B) =>  #t
```

而类型转换过程分别是 `char-downcase` 和`char-upcase`：

```scheme
(char-downcase #\A) =>  #\a
(char-upcase #\a)   =>  #\A
```





symbols通常在Scheme程序中被用来当做变量的标识，求值后可以得到变量所代表的值

我们还可以使用`define` 将symbol 类型的数据 如`xyz`当成一个全局的变量来使用：

```scheme
(define xyz 9)
```

这样可以就创建了一个值为9的变量`xyz`。

如果想改变`xyz`中的值可以用`set!`来实现：

```scheme
(set! xyz #\c)
```

Scheme 的 symbols 类型通常都是不区分大小写的。



string

Strings是自运算类型。

```scheme
"Hello, World!"
=>  "Hello, World!"
```

还可以通过向`string` 过程传递一组字符并返回由它们合并成的字符串：

```scheme
(string #\h #\e #\l #\l #\o)
=>  "hello"
```

现在让我们定义一个全局字符串变量 `greeting`。

```scheme
(define greeting "Hello; Hello!")
```

一个给定字符串数据中的字符可以分别被访问和更改。 通过向`string-ref`过程传递一个字符串和一个从0开始的索引号，可以返回该字符串指定索引号位置的字符。

```scheme
(string-ref greeting 0)
=>  #\H
```



`string-ref` 返回索引 `(string-ref greeting 0)`   

`string-append` 在一个现有的字符串上追加其它字符串  `(string-append "a" "b" "c")`

`string?`检测一个值是否是字符串类型的过程是

`string-set!`就可以替换字符串指定索引处的字符 `(string-set! hello 1 #\a)`

`make-vector` && `make-string` 

```scheme
> (define v (make-vector 5))
> v
'#(0 0 0 0 0)
> (define v (make-string 5))
> v
"\u0000\u0000\u0000\u0000\u0000"
```

### Vectors (向量)

Vectors 的元素可以是任何类型。同样`vector-ref` 和`vector-set!`分别可以访问和修改向量元素。

### Dotted pairs(点对) 和 lists(列表)

点对的第一个值被称作car，第二个值被称作cdr，而将两个值组合成点值对的过程是cons。

```scheme
> (define x (cons 1 5))
> x
'(1 . 5)
> (car x)
1
> (cdr x)
5
```

点对的元素可以通过修改器过程`set-car!` 和`set-cdr!`来进行修改：



### hash

```scheme
> (define ht (hash "key" 1))                                                                  > ht
'#hash(("key" . 1))
```





### 数据类型的转换

字符还可以通过使用`char->integer`来转换成整型，同样的整型也可以通过`integer->char`被转换成字符。(字符转换成整型得到的结果通常是这个字符的ascii码值。)

```scheme
(char->integer #\d) =>  100
(integer->char 50)  =>  #\2
```

字符串可以被转换成等价的字符列表。

```scheme
(string->list "hello") =>  (#\h #\e #\l #\l #\o)
```

其它的转换过程也都是一样的风格`list->string`， `vector->list` 和 `list->vector`。

