# Parser Test: `class_head`

## AST
```
(class (name 1:7-1:10 Foo))
(class (name 2:7-2:10 Bar))
(base class (name 4:12-4:18 Animal))
(class (name 5:7-5:10 Dog) (extends (var 5:19-5:25 Animal)))
(class (name 6:7-6:10 Cat) (extends (var 6:19-6:25 Animal)))
```
