## λ演算（lambda calculus）

主要参考

[认知科学家写给小白的Lambda演算](https://zhuanlan.zhihu.com/p/30510749)

[面向眼科医生的λ演算入门教程](https://goldengrape.github.io/categories/jiao-cheng/)

环境：http://www.cburch.com/lambda/ （这是一个Lambda Calculator解释器（lambda calculus interpreter，LCI），如果搜索online lambda calculus interpreter能够找到很多类似的。）

要输入λ比较麻烦，所以在这个解释器中，使用左斜杠 `\` 来表示 λ

### 变量

> **variables有三种：metavariable & bound variable & free variables.**
>
> 在` f(x)=ax^2+bx+c ` 中，x，写在了` f(x)` 的括号里面，这个x可以叫做metavariable，在函数的定义式子里面已经明确告诉你，函数定义里面的x如何变化要去看metavariable到底是什么，所以` ax^2+bx+c `式子里的 x 叫做 bound variables，是约束变量，是被bound 到 metavariable 上的。
>
> 而对于式子中的 a,b,c，并不在这个函数内部定义，是在函数外部定义的，比如题目中写的，或者老师写在了黑板上，abc也是可变的，但怎么变这个函数定义式没说，是自由的，所以叫做自由变量 free variables。所有没有bound到metavariables上的变量都是free的。
>
> 而如果是 `g(a,b,c)=ax^2+bx+c` 那么就是另外一个函数了，里面的 bound variables 是a,b,c，而x反而变成了 free variable.

- metavariable是写在λ和点.之间的那个

- 函数表达式里和 metavariable 绑定的那个是 bound variable

- 函数表达式里没有被绑住，可以自由变来变去的那些都是free variables.

- 对于 lambda 表达式：应该替换掉的是 bound variables，如果一个变量是 free variables 就要去它的上一层找值。

  > λx.只声明了一件事情，λ和点. 之间的x是 metavariable。
  >
  > 在点.后面的是函数的定义表达式，如果变量跟 metavariable 是绑定的，就是bound variable。
  >
  > 比如：
  >
  > ```
  > (λx.x) HelloWorld  
  > ```
  >
  > 相当于 `f(x)=x` ，求 x=HelloWorld，就是将 x 替换成 HelloWorld
  >
  > λx.中间是metavariable x，λx.后面是函数定义的表达式x，这里面只有一个变量x，与metavariable是相同的，是bound variable。用 Hello 替换掉 bound variable。

**继续介绍：**

在λ演算中，一行符号被叫做表达式。例如表达式长成这样子：(λx.xy) (ab)。表达式只包含以下符号：

- 单个字母（abcd...)，被称作变量。一个表达式可以是单个字母，或多个字母。一般的，我们可以把两个表达式写在一起组成一个新的表达式。
- 括号()。括号表明表达式被括起来的部分是一个整体（就像句子中的括号表示这部分是一个整体）。当我们没有括号时，我们从左到右分析表达式。
- 希腊字母λ(发音：Lambda)，和点(.)。 λ和点，我们可以描述函数。函数由λ和变量开头，跟上一个点，然后是表达式。λ没有任何特别的含义，它只是说函数由此开始。在λ后面，在点之前的字母，我们称作的变量，点之前的部分，被称作头部(head)，点后面的表达式，被称作体(body)部。

### Lambda Expression

λ表达式Lambda Expression，有4种可能的写法

```scheme
E->ID
E->λID.E
E->E E
E->(E)
```

#### 括号解释

解析过程就是将头部一个变量去掉，然后将它所有在体部的出现的这个变量替换成写在函数后边跟着的表达式。如下，括号只是表明表达式被括起来的部分是一个整体。

> 替换掉bound variables，这个叫β reduction

```scheme
((λx.(λy.x)) x) y ⇒ (λy.x) y ⇒ x
```



#### 后继数

这里有数学中的`后继数（successor number）`的概念，后继数可以通过`后继函数`得到。例如：0 的后继数是 1，1 的后继数是 2，于是有了后继函数 f(x)=x+1 ，我们可以利用后继函数通过[ 递归定义 ](https://zh.wikipedia.org/wiki/递归定义)方式定义自然数，所以会有以下推导：

现在考虑一种有意思的情况：数字 n 表示 x 前面有 n 个 f

```scheme
0	=	λf.λx.x
1	=	λf.λx.f(x)
2	=	λf.λx.f (f x)	
3	=	λf.λx.f (f (f x))	
```

后继函数：S，即 `S = λn.λy.λx.y (n y x)`。

推导过程如下：

```
S
⇒	λn.λy.λx.y (n y x)

0
⇒	λf.λx.x

1
⇒	λf.λx.f(x)

S 0
(λn.λy.λx.y (n y x))  (λs.λz.z)
(λy.λx.y ((λs.λz.z) y x)) 
(λy.λx.y ((λz.z) x)) 
(λy.λx.y (x)  ------->  λf.λx.f(x)  即为 1

S 1
(λn.λy.λx.y (n y x))  (λs.λz.s.z)
(λy.λx.y ((λs.λz.s.z) y x)) 
(λy.λx.y ((λz.y.z) x)) 
(λy.λx.y (y(x)))   -----> λf.λx.f(f(x)) 即为 2
```



我们发现运用上面的 后继函数 S 仍然可以进行推导：

```
3S5
⇒	(λ sz.s(s(s(z)))) (λ abc.b(abc)) (λ xy.x(x(x(x(x(y))))))
⇒	8:λ xy.x(x(x(x(x(x(x(x(y))))))))

```





### 数据结构

最基本的数据结构就是一对数据了，从数据对又可以衍生出二叉树、链表，在此基础上又可以衍生出各种各样的数据结构。如果有了一对数据，就要能够提取其第一个元和第二个元。因此需要有三个操作，pair用来构造，first, second用来读取。

> 回顾 chapter 7 中的 `first` 、`second`，就是从 pair 中取出。
>
> ```scheme
> (define first
>     (lambda (p) 
>         (car p)))
> 
> (define second
>     (lambda (p) 
>         (car (cdr p))))
> ```



#### pair

```
pair=λx.λy.λz.(z x y)
first=λp.p (λx.λy.x)
second=λp.p (λx.λy.y)
```

试试

```
pair a b
=(λx.λy.λz.(z x y)) a b
=λz.(z a b)
```

取 first

> 推导时候可以感觉到，λp.p 和 λz.z 应该是个小技巧，可以用来把后面的东西提前面来用。

```
first (pair a b)
=λp.p (λx.λy.x) λz.(z a b)
=λz.(z a b) (λx.λy.x)
=(λx.λy.x) a b
=a
```



#### list

有了二，就可以有三有多了。比如三元的 list：

```
list3=pair a (pair b (pair c nil)
```

nil表示一个表结尾标记，比如在C语言中用\0作为字符串的结尾。

如果要取 list 中的第三个位置，那就first( second (second list) )   类似 `car   cdr`

> 有pair以后，其实就可以定义整数之外的数了， 
>
> - 有理数可以写成一对整数(分子, 分母)，比如0.8=4/5，用(4,5)这样的结构就可以表示了。
> - 复数可以写成(实部, 虚部)的pair 有理复数，可以加入一些标记，比如用c作为复数标记，用r作为有理数标记，那么一个有理复数可以使用这样的结构：(c,(实部, 虚部))，继续展开是(c, ( (r,(分子,分母), (r,(分子,分母) ) ) 甚至用一个表来模拟8位字节

**只要数的定义有相应的运算法则可以操作就可以了。 有了二，就有了数据结构。**



#### 逻辑运算

##### `true` or `false`

什么是真，什么是假，哲学家也许一直都在试图定义。**但对逻辑学或者数学来说，真假只是一次选择，if True then This, if False then That.** 

此处实现的 true or false： 都是接收两个参数，true返回前一个，false返回后一个。

```
true=λx.λy.x
false=λx.λy.y
```

##### `and` or `not`

有了not和and，就有与非门，用来实现各种逻辑运算。

```
and=λp.λq.(p q p)
or=λa.λb.a (λx.λy.x) b
not=λp.(p false true)
```

看 not 的结构，λp.(p...)的模式以前出现过，就是把输入调用到前面来。

```
not true = true false true = false
```

true 的作用是把输入的两个参数前面那个挑出来，把第一个true当作函数，把后面的false true当作两个输入参数，挑出前面那个，就是false，于是not true=false。妙啊。

##### if

貌似 true this that 和 false this that 就已经可以达到 if 语句要求了。但是如何在不知道条件是true还是false的时候把条件加入到this that前面呢？

其实也跟 not 类似，用上λp.(p...)的结构

```
if=λp.λa.λb.(p a b)
```

下面一个 `IS-ZERO`的栗子便是：如果是0，返回真，如果不是0, 返回假。

```scheme
IS-ZERO = λa.a FALSE TRUE

;------------------------------
IS-ZERO 0
= (λa.a FALSE TRUE) (λsz.z)
= (λsz.z) FALSE TRUE
= (λz.z)  TRUE
= TRUE

IS-ZERO 1 
= λsz.s(z) FALSE TRUE
= λz.FALSE(z) TRUE
= FALSE
```



##### `>=`

已知一个数字是0，我们可以找出另一个数字是不是大于等于0，我们可以这样描述：

GREATER-OR-EQUAL n m = λ nm. IS-ZERO (n P m)			(P 为 前继函数，推导见[此处](https://zhuanlan.zhihu.com/p/30510749))

换句话说，我们应用n次前继函数给m。如果n和m相等，结果是0。如果n比m大，结果是0。只有在n比m小的情况下，结果大于0。

**相等性**

使用≥，我们可以确定相等性：n=m 如果m≥n且n≥m。我们可以这样描述：

```
EQUAL n m 
= λ n m. AND (GREATER-OR-EQUAL(n m) GREATER-OR-EQUAL(m n))
= λ n m. AND (IS-ZERO(n P m) IS-ZERO(m P n))
```



### 递归

现在思考一下 Fibonacci数列

```
F(0)=0
F(1)=1
F(i)=F(i-1)+F(i-2)
```

#### ω combinator

这种递归的方法只有当函数有名字的时候才能实现。所以如果是用 lambda calculus，这种匿名函数语言，是没有办法使用带有名字的召唤术。

那没有名字该如何调用呢？

有办法！ω combinator

比如

```
ω=(λx.(x x))
```

那么：

```
ω ω=(λx.(x x)) ω
```

做β reduction，就是用ω替代x x中的x，变成 ω ω。于是

```
ω ω=(λx.(x x)) ω =ω ω
```

看，循环了，而且永久进行下去不会停歇。

这个叫做ω combinator，所谓combinator，就是 lambda expression 里面都是 bound variables，没有 free 的。

ω combinator 并不能执行有意义的操作，它产生了循环，但定义中只有x，所以它顶多能够调用自身，并没有办法将其他的操作加入到循环体内部。我们需要还需要更多的变量，需要的是Y combinator。

#### Y combinator

```
Y=(λxy.( y (x x y)))  (λxy.( y (x x y)))
```

这就是Y combinator，Y由两个重复的部分构成，直接看过去你就会发现这个并没有进行完全的化简，后面的部分并没有代入到前面部分里。为了容易描述，我擅自命名重复的部分为Ypart，那么Y=Ypart Ypart

```scheme
Y foo						;foo表示另外一个函数
= (λx.λy.( y (x x y)))     (λx.λy.( y (x x y)))     foo		;故意写开一点，容易辨识
= (λx.λy.( y (x x y)))     Ypart		foo		;把Y看成两部分，在加上foo，是3个expression
= λy.( y (Ypart Ypart y)) foo				;把 Ypart 代入，替换掉x
= foo (Ypart Ypart foo)						;把 foo 代入，替换掉y
= foo (Y foo)								;Ypart Ypart不就是Y本身么
```

因此！上面的推导表示：`Y foo	= foo (Y foo) `

```
Y foo 
= foo (Y foo)
= foo (foo (Y foo))
= foo (foo (foo (Y foo)))
```

循环出现了！！！道生一，一生二，二生三，三生万物。







### 柯里化

我们可以使用高阶函数将一个接受多参数的函数转换成接受一个单一参数（最初函数的第一个参数）的函数。 具体地说，给定函数f(x，y)，我们可以定义函数g，使得g(x)(y)等价于f(x，y)。 这里，g是一个高阶函数，它接受单个参数x，并返回另一个接受单个参数y的函数。 这种转变叫做`currying`函数柯里化。
 例如，我们可以定义一个`pow`函数的柯里化版本：

```python
>>> def curried_pow(x):
        def h(y):
            return pow(x, y)
        return h
>>> curried_pow(2)(3)
8
```

#### 使用栗子

例如，`map`模式将单参数函数应用于一系列值。 在随后的章节中，我们将看到更多的`map`模式的例子。至于现在，我们可以在一个函数中实现这个模式：

```python
>>> def map_to_range(start, end, f):
        while start < end:
            print(f(start))
            start = start + 1
```

我们可以使用`map_to_range`和`curried_pow`来计算2的前10个幂，而不是专门写一个函数：

```python
>>> map_to_range(0, 10, curried_pow(2))
1
2
4
8
16
32
64
128
256
512
```

我们可以类似地使用相同的两个函数来计算其他数字的幂函数。 `currying`可以让我们做到，它不需要为我们希望计算的幂的每一个数写一个特定的函数。