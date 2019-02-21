# Parser Test: `class_member_base_method`

## AST
```
(class
 (name 1:7-1:8 C)
 (base fun (name 2:12-2:13 f) (type (var 2:19-2:20 T)))
 (base fun
  (name 3:12-3:13 f)
  (param (var 3:14-3:15 a))
  (type (var 3:20-3:21 T)))
 (base fun
  (name 4:12-4:13 f)
  (param (var 4:14-4:15 a))
  (param (var 4:17-4:18 b))
  (type (var 4:23-4:24 T)))
 (base fun
  (name 5:12-5:13 f)
  (param (var 5:14-5:15 a))
  (param (var 5:17-5:18 b))
  (param (var 5:20-5:21 c))
  (type (var 5:26-5:27 T)))
 (base fun
  (name 6:12-6:13 f)
  (param (var 6:14-6:15 a))
  (type (var 6:21-6:22 T)))
 (base fun
  (name 7:12-7:13 f)
  (param (var 7:14-7:15 a))
  (param (var 7:17-7:18 b))
  (type (var 7:24-7:25 T)))
 (base fun
  (name 8:12-8:13 f)
  (param (var 8:14-8:15 a))
  (param (var 8:17-8:18 b))
  (param (var 8:20-8:21 c))
  (type (var 8:27-8:28 T)))
 (base fun
  (name 9:12-9:13 f)
  (param (var 9:14-9:15 a) (type (var 9:17-9:18 T)))
  (type (var 9:23-9:24 T)))
 (base fun
  (name 10:12-10:13 f)
  (param (var 10:14-10:15 a) (type (var 10:17-10:18 T)))
  (param (var 10:20-10:21 b) (type (var 10:23-10:24 U)))
  (type (var 10:29-10:30 T)))
 (base fun
  (name 11:12-11:13 f)
  (param (var 11:14-11:15 a) (type (var 11:17-11:18 T)))
  (param (var 11:20-11:21 b) (type (var 11:23-11:24 U)))
  (param (var 11:26-11:27 c) (type (var 11:29-11:30 V)))
  (type (var 11:35-11:36 T)))
 (base fun
  (name 12:12-12:13 f)
  (param (var 12:14-12:15 a) (type (var 12:17-12:18 T)))
  (type (var 12:24-12:25 T)))
 (base fun
  (name 13:12-13:13 f)
  (param (var 13:14-13:15 a) (type (var 13:17-13:18 T)))
  (param (var 13:20-13:21 b) (type (var 13:23-13:24 U)))
  (type (var 13:30-13:31 T)))
 (base fun
  (name 14:12-14:13 f)
  (param (var 14:14-14:15 a) (type (var 14:17-14:18 T)))
  (param (var 14:20-14:21 b) (type (var 14:23-14:24 U)))
  (param (var 14:26-14:27 c) (type (var 14:29-14:30 V)))
  (type (var 14:36-14:37 T)))
 (base fun (name 15:12-15:16 base) (type (var 15:22-15:23 T)))
 (base fun
  (name 16:12-16:13 f)
  (param (var 16:14-16:15 T))
  (type (fun (param (var 16:24-16:25 T)) (var 16:30-16:31 T)))))
```
