参考资料

[[MIT] 6.001 计算机程序的构造与解释 (Structure and Interpretation, 1986)](https://www.bilibili.com/video/av8515129)

[MIT 课程官网](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/readings/)

[SICP book 官网](https://mitpress.mit.edu/sites/default/files/sicp/index.html)

[北大“程序设计技术和方法”幻灯片](https://www.math.pku.edu.cn/teachers/qiuzy/progtech/slides/index.php)



对于一些需要导入整个文件来说，使用函数load会出错，解决办法见此：https://stackoverflow.com/questions/4809433/including-an-external-file-in-racket

对被引入文件加上

```scheme
(provide (fortytwo det))
```

对引入文件加上诸如：

```scheme
(require "xxx.rkt")
```





