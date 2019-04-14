# Parser Test: `class_member_base_method`

## AST
```
(class
 (name 1:7-1:8 C)
 (base fun (name 2:12-2:13 f) (type (var 2:17-2:18 T)))
 (base fun
  (name 3:12-3:13 f)
  (param (var 3:14-3:15 a))
  (type (var 3:18-3:19 T)))
 (base fun
  (name 4:12-4:13 f)
  (param (var 4:14-4:15 a))
  (param (var 4:17-4:18 b))
  (type (var 4:21-4:22 T)))
 (base fun
  (name 5:12-5:13 f)
  (param (var 5:14-5:15 a))
  (param (var 5:17-5:18 b))
  (param (var 5:20-5:21 c))
  (type (var 5:24-5:25 T)))
 (base fun
  (name 6:12-6:13 f)
  (param (var 6:14-6:15 a))
  (type (var 6:19-6:20 T)))
 (base fun
  (name 7:12-7:13 f)
  (param (var 7:14-7:15 a))
  (param (var 7:17-7:18 b))
  (type (var 7:22-7:23 T)))
 (base fun
  (name 8:12-8:13 f)
  (param (var 8:14-8:15 a))
  (param (var 8:17-8:18 b))
  (param (var 8:20-8:21 c))
  (type (var 8:25-8:26 T)))
 (base fun
  (name 9:12-9:13 f)
  (param (var 9:14-9:15 a) (type (var 9:17-9:18 T)))
  (type (var 9:21-9:22 T)))
 (base fun
  (name 10:12-10:13 f)
  (param (var 10:14-10:15 a) (type (var 10:17-10:18 T)))
  (param (var 10:20-10:21 b) (type (var 10:23-10:24 U)))
  (type (var 10:27-10:28 T)))
 (base fun
  (name 11:12-11:13 f)
  (param (var 11:14-11:15 a) (type (var 11:17-11:18 T)))
  (param (var 11:20-11:21 b) (type (var 11:23-11:24 U)))
  (param (var 11:26-11:27 c) (type (var 11:29-11:30 V)))
  (type (var 11:33-11:34 T)))
 (base fun
  (name 12:12-12:13 f)
  (param (var 12:14-12:15 a) (type (var 12:17-12:18 T)))
  (type (var 12:22-12:23 T)))
 (base fun
  (name 13:12-13:13 f)
  (param (var 13:14-13:15 a) (type (var 13:17-13:18 T)))
  (param (var 13:20-13:21 b) (type (var 13:23-13:24 U)))
  (type (var 13:28-13:29 T)))
 (base fun
  (name 14:12-14:13 f)
  (param (var 14:14-14:15 a) (type (var 14:17-14:18 T)))
  (param (var 14:20-14:21 b) (type (var 14:23-14:24 U)))
  (param (var 14:26-14:27 c) (type (var 14:29-14:30 V)))
  (type (var 14:34-14:35 T)))
 (base fun (name 15:12-15:16 base) (type (var 15:20-15:21 T)))
 (base fun
  (name 16:12-16:13 f)
  (param (var 16:14-16:15 T))
  (type (fun (param (var 16:22-16:23 T)) (var 16:26-16:27 T)))))
```
