# Parser Test: `class_member_field`

## AST
```
(class
 (name 1:7-1:9 C1)
 (field (name 2:3-2:4 a) (var 2:6-2:7 A))
 (field (name 3:3-3:4 b) (var 3:6-3:7 B))
 (field (name 4:3-4:4 c) (var 4:6-4:7 C))
 (field (name 5:3-5:7 base) (var 5:9-5:10 T))
 (field (name 6:3-6:4 x) (var 6:6-6:7 T))
 (field (name 7:3-7:4 y) (var 7:6-7:7 U))
 (field (name 8:3-8:4 z) (var 8:6-8:7 V))
 (field (name 9:3-9:6 foo) (var 9:8-9:11 Bar))
 (field (name 10:3-10:6 qux) (var 10:8-10:11 Lit)))
(class
 (name 13:7-13:9 C2)
 (field (name 14:3-14:4 a) (var 14:6-14:7 A))
 (field (name 15:3-15:4 b) (var 15:6-15:7 B))
 (field (name 16:3-16:4 c) (var 16:6-16:7 C))
 (field (name 17:3-17:7 base) (var 17:9-17:10 T))
 (field (name 18:3-18:4 x) (var 18:6-18:7 T))
 (field (name 19:3-19:4 y) (var 19:6-19:7 U))
 (field (name 20:3-20:4 z) (var 20:6-20:7 V))
 (field (name 21:3-21:6 foo) (var 21:8-21:11 Bar))
 (field (name 22:3-22:6 qux) (var 22:8-22:11 Lit)))
```
