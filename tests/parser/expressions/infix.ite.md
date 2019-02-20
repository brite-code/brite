# Parser Test: `infix`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (add (var 2:3-2:4 a) (var 2:7-2:8 b))
  (add (add (var 3:3-3:4 a) (var 3:7-3:8 b)) (var 3:11-3:12 c))
  (sub (var 4:3-4:4 a) (var 4:7-4:8 b))
  (sub (sub (var 5:3-5:4 a) (var 5:7-5:8 b)) (var 5:11-5:12 c))
  (mul (var 6:3-6:4 a) (var 6:7-6:8 b))
  (mul (mul (var 7:3-7:4 a) (var 7:7-7:8 b)) (var 7:11-7:12 c))
  (div (var 8:3-8:4 a) (var 8:7-8:8 b))
  (div (div (var 9:3-9:4 a) (var 9:7-9:8 b)) (var 9:11-9:12 c))
  (rem (var 10:3-10:4 a) (var 10:7-10:8 b))
  (rem (rem (var 11:3-11:4 a) (var 11:7-11:8 b)) (var 11:11-11:12 c))
  (eq (var 12:3-12:4 a) (var 12:8-12:9 b))
  (eq (eq (var 13:3-13:4 a) (var 13:8-13:9 b)) (var 13:13-13:14 c))
  (neq (var 14:3-14:4 a) (var 14:8-14:9 b))
  (neq (neq (var 15:3-15:4 a) (var 15:8-15:9 b)) (var 15:13-15:14 c))
  (lt (var 16:3-16:4 a) (var 16:7-16:8 b))
  (lt (lt (var 17:3-17:4 a) (var 17:7-17:8 b)) (var 17:11-17:12 c))
  (lte (var 18:3-18:4 a) (var 18:8-18:9 b))
  (lte (lte (var 19:3-19:4 a) (var 19:8-19:9 b)) (var 19:13-19:14 c))
  (gt (var 20:3-20:4 a) (var 20:7-20:8 b))
  (gt (gt (var 21:3-21:4 a) (var 21:7-21:8 b)) (var 21:11-21:12 c))
  (gte (var 22:3-22:4 a) (var 22:8-22:9 b))
  (gte (gte (var 23:3-23:4 a) (var 23:8-23:9 b)) (var 23:13-23:14 c))
  (sub (add (var 24:3-24:4 a) (var 24:7-24:8 b)) (var 24:11-24:12 c))
  (add (sub (var 25:3-25:4 a) (var 25:7-25:8 b)) (var 25:11-25:12 c))
  (add (var 26:3-26:4 a) (mul (var 26:7-26:8 b) (var 26:11-26:12 c)))
  (add (mul (var 27:3-27:4 a) (var 27:7-27:8 b)) (var 27:11-27:12 c))
  (add (var 28:3-28:4 a) (div (var 28:7-28:8 b) (var 28:11-28:12 c)))
  (add (div (var 29:3-29:4 a) (var 29:7-29:8 b)) (var 29:11-29:12 c))
  (div (mul (var 30:3-30:4 a) (var 30:7-30:8 b)) (var 30:11-30:12 c))
  (mul (div (var 31:3-31:4 a) (var 31:7-31:8 b)) (var 31:11-31:12 c))
  (add
   (add (var 32:3-32:4 a) (mul (var 32:7-32:8 b) (var 32:11-32:12 c)))
   (var 32:15-32:16 d))
  (add
   (mul (var 33:3-33:4 a) (var 33:7-33:8 b))
   (mul (var 33:11-33:12 c) (var 33:15-33:16 d)))
  (add (exp (var 34:3-34:4 a) (var 34:7-34:8 b)) (var 34:11-34:12 c))
  (add (var 35:3-35:4 a) (exp (var 35:7-35:8 b) (var 35:11-35:12 c)))
  (mul (exp (var 36:3-36:4 a) (var 36:7-36:8 b)) (var 36:11-36:12 c))
  (mul (var 37:3-37:4 a) (exp (var 37:7-37:8 b) (var 37:11-37:12 c)))
  (gt (var 38:3-38:4 a) (add (var 38:7-38:8 b) (var 38:11-38:12 c)))
  (gt (add (var 39:3-39:4 a) (var 39:7-39:8 b)) (var 39:11-39:12 c))
  (lt (var 40:3-40:4 a) (add (var 40:7-40:8 b) (var 40:11-40:12 c)))
  (lt (add (var 41:3-41:4 a) (var 41:7-41:8 b)) (var 41:11-41:12 c))
  (gte (var 42:3-42:4 a) (add (var 42:8-42:9 b) (var 42:12-42:13 c)))
  (gte (add (var 43:3-43:4 a) (var 43:7-43:8 b)) (var 43:12-43:13 c))
  (lte (var 44:3-44:4 a) (add (var 44:8-44:9 b) (var 44:12-44:13 c)))
  (lte (add (var 45:3-45:4 a) (var 45:7-45:8 b)) (var 45:12-45:13 c))
  (eq (add (var 46:3-46:4 a) (var 46:7-46:8 b)) (var 46:12-46:13 c))
  (eq (var 47:3-47:4 a) (add (var 47:8-47:9 b) (var 47:12-47:13 c)))
  (neq (add (var 48:3-48:4 a) (var 48:7-48:8 b)) (var 48:12-48:13 c))
  (neq (var 49:3-49:4 a) (add (var 49:8-49:9 b) (var 49:12-49:13 c)))
  (and (var 50:3-50:4 a) (var 50:8-50:9 b))
  (and (and (var 51:3-51:4 a) (var 51:8-51:9 b)) (var 51:13-51:14 c))
  (or (or (var 52:3-52:4 a) (var 52:8-52:9 b)) (var 52:13-52:14 c))
  (or (and (var 53:3-53:4 a) (var 53:8-53:9 b)) (var 53:13-53:14 c))
  (or (var 54:3-54:4 a) (and (var 54:8-54:9 b) (var 54:13-54:14 c)))
  (and
   (and (and (var 55:3-55:4 a) (var 55:8-55:9 b)) (var 55:13-55:14 c))
   (var 55:18-55:19 d))
  (or
   (var 56:3-56:4 a)
   (and (and (var 56:8-56:9 b) (var 56:13-56:14 c)) (var 56:18-56:19 d)))
  (or
   (and (var 57:3-57:4 a) (var 57:8-57:9 b))
   (and (var 57:13-57:14 c) (var 57:18-57:19 d)))
  (or
   (and (and (var 58:3-58:4 a) (var 58:8-58:9 b)) (var 58:13-58:14 c))
   (var 58:18-58:19 d))
  (or
   (or (and (var 59:3-59:4 a) (var 59:8-59:9 b)) (var 59:13-59:14 c))
   (var 59:18-59:19 d))
  (or
   (or (var 60:3-60:4 a) (and (var 60:8-60:9 b) (var 60:13-60:14 c)))
   (var 60:18-60:19 d))
  (or
   (or (var 61:3-61:4 a) (var 61:8-61:9 b))
   (and (var 61:13-61:14 c) (var 61:18-61:19 d)))
  (or
   (or (or (var 62:3-62:4 a) (var 62:8-62:9 b)) (var 62:13-62:14 c))
   (var 62:18-62:19 d))
  (add (var 63:3-63:4 a) (mul (var 63:7-63:8 b) (var 63:11-63:12 c)))
  (mul
   (wrap 64:3-64:10 (add (var 64:4-64:5 a) (var 64:8-64:9 b)))
   (var 64:13-64:14 c))))
```
