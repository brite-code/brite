open TestFramework

let () = suite "Unify" (fun () -> (
  let cases = [
    ("unify((∅), boolean, boolean)", "(∅)", []);
    ("unify((∅), number, number)", "(∅)", []);
    ("unify((∅), string, string)", "(∅)", []);
    ("unify((a), number, number)", "(a)", []);
    ("unify((a, b), number, number)", "(a, b)", []);
    ("unify((a, b, c), number, number)", "(a, b, c)", []);
    ("unify((∅), number → number, number → number)", "(∅)", []);
    ("unify((∅), number → string, number → string)", "(∅)", []);
    ("unify((∅), number → string → boolean, number → string → boolean)", "(∅)", []);
    ("unify((∅), number → number, number → string)", "(∅)", ["number ≢ string"]);
    ("unify((∅), number → number, string → number)", "(∅)", ["string ≢ number"]);
    ("unify((∅), number → number, string → string)", "(∅)", ["string ≢ number"; "number ≢ string"]);
    ("unify((∅), number → number → number, string → string → string)", "(∅)", ["string ≢ number"; "string ≢ number"; "number ≢ string"]);
    ("unify((∅), (number → number) → number, (string → string) → string)", "(∅)", ["number ≢ string"; "string ≢ number"; "number ≢ string"]);
    ("unify((∅), number, number → number)", "(∅)", ["number ≢ number → number"]);
    ("unify((∅), number → number, number)", "(∅)", ["number → number ≢ number"]);
    ("unify((a = number), a, number)", "(a = number)", []);
    ("unify((a = number), number, a)", "(a = number)", []);
    ("unify((a = number), a, string)", "(a = number)", ["number ≢ string"]);
    ("unify((a = number), string, a)", "(a = number)", ["string ≢ number"]);
    ("unify((a ≥ number), a, number)", "(a ≥ number)", []);
    ("unify((a ≥ number), number, a)", "(a ≥ number)", []);
    ("unify((a ≥ number), a, string)", "(a ≥ number)", ["number ≢ string"]);
    ("unify((a ≥ number), string, a)", "(a ≥ number)", ["string ≢ number"]);
    ("unify((b = number, a = b), a, number)", "(b = number, a = b)", []);
    ("unify((b = number, a = b), number, a)", "(b = number, a = b)", []);
    ("unify((b = number, a = b), a, string)", "(b = number, a = b)", ["number ≢ string"]);
    ("unify((b = number, a = b), string, a)", "(b = number, a = b)", ["string ≢ number"]);
    ("unify((b = number, a ≥ b), a, number)", "(b = number, a ≥ b)", []);
    ("unify((b = number, a ≥ b), number, a)", "(b = number, a ≥ b)", []);
    ("unify((b = number, a ≥ b), a, string)", "(b = number, a ≥ b)", ["number ≢ string"]);
    ("unify((b = number, a ≥ b), string, a)", "(b = number, a ≥ b)", ["string ≢ number"]);
    ("unify((b ≥ number, a = b), a, number)", "(b ≥ number, a = b)", []);
    ("unify((b ≥ number, a = b), number, a)", "(b ≥ number, a = b)", []);
    ("unify((b ≥ number, a = b), a, string)", "(b ≥ number, a = b)", ["number ≢ string"]);
    ("unify((b ≥ number, a = b), string, a)", "(b ≥ number, a = b)", ["string ≢ number"]);
    ("unify((b ≥ number, a ≥ b), a, number)", "(b ≥ number, a ≥ b)", []);
    ("unify((b ≥ number, a ≥ b), number, a)", "(b ≥ number, a ≥ b)", []);
    ("unify((b ≥ number, a ≥ b), a, string)", "(b ≥ number, a ≥ b)", ["number ≢ string"]);
    ("unify((b ≥ number, a ≥ b), string, a)", "(b ≥ number, a ≥ b)", ["string ≢ number"]);
    ("unify((a), a, a)", "(a)", []);
    ("unify((a ≥ ⊥), a, a)", "(a)", []);
    ("unify((a = ⊥), a, a)", "(a = ⊥)", []);
    ("unify((a, b = a, c = b), b, c)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), c, b)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), a, b)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), a, c)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), b, a)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), c, a)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), b, a)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), b, b)", "(a, b = a, c = b)", []);
    ("unify((a, b = a, c = b), c, c)", "(a, b = a, c = b)", []);
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, c)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), c, b)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), a, b)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), a, c)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, a)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), c, a)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, a)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), b, b)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    (* TODO: ("unify((a, b = ∀z.a, c = ∀z.b), c, c)", "(a, b = ∀z.a, c = ∀z.b)", []); *)
    ("unify((a), a, number)", "(a = number)", []);
    ("unify((a), number, a)", "(a = number)", []);
    ("unify((a ≥ ⊥), a, number)", "(a = number)", []);
    ("unify((a ≥ ⊥), number, a)", "(a = number)", []);
    ("unify((a = ⊥), a, number)", "(a = ⊥)", ["number ≢ ⊥"]);
    ("unify((a = ⊥), number, a)", "(a = ⊥)", ["number ≢ ⊥"]);
    ("unify((b ≥ ⊥, a ≥ b), a, number)", "(b = number, a ≥ b)", []);
    ("unify((b ≥ ⊥, a ≥ b), number, a)", "(b = number, a ≥ b)", []);
    ("unify((b ≥ ⊥, a = b), a, number)", "(b = number, a = b)", []);
    ("unify((b ≥ ⊥, a = b), number, a)", "(b = number, a = b)", []);
    ("unify((b = ⊥, a ≥ b), a, number)", "(b = ⊥, a ≥ b)", ["number ≢ ⊥"]);
    ("unify((b = ⊥, a ≥ b), number, a)", "(b = ⊥, a ≥ b)", ["number ≢ ⊥"]);
    ("unify((b = ⊥, a = b), a, number)", "(b = ⊥, a = b)", ["number ≢ ⊥"]);
    ("unify((b = ⊥, a = b), number, a)", "(b = ⊥, a = b)", ["number ≢ ⊥"]);
    ("unify((b, a = b → b), a, b)", "(b, a = b → b)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, a = b → b), b, a)", "(b, a = b → b)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, c = b → b, a = c), a, b)", "(b, c = b → b, a = c)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, c = b → b, a = c), b, a)", "(b, c = b → b, a = c)", ["Infinite type since `b` occurs in `b → b`."]);
    ("unify((b, c = b → b, a = c → c), a, b)", "(b, c = b → b, a = c → c)", ["Infinite type since `b` occurs in `c → c`."]);
    ("unify((b, c = b → b, a = c → c), b, a)", "(b, c = b → b, a = c → c)", ["Infinite type since `b` occurs in `c → c`."]);
    ("unify((a, b), a, b)", "(a, b = a)", []);
    ("unify((a, b), b, a)", "(b, a = b)", []);
    ("unify((a, b), a, b → b)", "(b, a = b → b)", []);
    ("unify((a, b), b → b, a)", "(b, a = b → b)", []);
    ("unify((a ≥ ∀x.x → x), a, number → number)", "(a = number → number)", []);
    ("unify((a ≥ ∀x.x → x), a, number → string)", "(a ≥ ∀x.x → x)", ["number ≢ string"]);
    ("unify((a = ∀x.x → x), a, number → number)", "(a = ∀x.x → x)", ["number → number ≢ ∀x.x → x"]);
    ("unify((a = ∀x.x → x), a, number → string)", "(a = ∀x.x → x)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b ≥ ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), b, a)", "(b ≥ ∀(z, x = z, y = x).y → z, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀(y, z).y → z), b, a)", "(b ≥ ∀(z, x = z, y = x).y → z, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀(y, z).y → z), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀(y, z).y → z), b, a)", "(b = ∀(z, x = z, y = x).y → z, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀(y, z).y → z), a, b)", "(a ≥ ∀x.x → x, b = ∀(y, z).y → z)", ["∀(y, x = y).x → x ≢ ∀(y, z).y → z"]);
    ("unify((a ≥ ∀x.x → x, b = ∀(y, z).y → z), b, a)", "(a ≥ ∀x.x → x, b = ∀(y, z).y → z)", ["∀(z, x = z, y = x).y → z ≢ ∀(y, z).y → z"]);
    ("unify((a = ∀x.x → x, b = ∀(y, z).y → z), a, b)", "(a = ∀x.x → x, b = ∀(y, z).y → z)", ["∀(y, x = y).x → x ≢ ∀(y, z).y → z"]);
    ("unify((a = ∀x.x → x, b = ∀(y, z).y → z), b, a)", "(a = ∀x.x → x, b = ∀(y, z).y → z)", ["∀(z, x = z, y = x).y → z ≢ ∀(y, z).y → z"]);
    ("unify((a ≥ ∀x.x → x, b = number → number), a, b)", "(a = number → number, b = number → number)", []);
    ("unify((a ≥ ∀x.x → x, b = number → number), b, a)", "(a = number → number, b = number → number)", []);
    ("unify((a, b = a → a), a, b)", "(a, b = a → a)", ["Infinite type since `a` occurs in `a → a`."]);
    ("unify((a, b = a → a), b, a)", "(a, b = a → a)", ["Infinite type since `a` occurs in `a → a`."]);
    ("unify((a, b = ∀c.c → a), a, b)", "(a, b = ∀c.c → a)", ["Infinite type since `a` occurs in `∀c.c → a`."]);
    ("unify((a, b = ∀c.c → a), b, a)", "(a, b = ∀c.c → a)", ["Infinite type since `a` occurs in `∀c.c → a`."]);
    ("unify((a = ⊥, b = ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a = ⊥, b = ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a = ⊥, b = ∀c.c → c), a, b)", "(a = ⊥, b = ∀c.c → c)", ["∀c.c → c ≢ ⊥"]);
    ("unify((a = ⊥, b = ∀c.c → c), b, a)", "(a = ⊥, b = ∀c.c → c)", ["∀c.c → c ≢ ⊥"]);
    ("unify((a, b), a, b)", "(a, b = a)", []);
    ("unify((a, b), b, a)", "(b, a = b)", []);
    ("unify((a ≥ ⊥, b ≥ ⊥), a, b)", "(a, b = a)", []);
    ("unify((a ≥ ⊥, b ≥ ⊥), b, a)", "(b, a = b)", []);
    ("unify((a = ⊥, b ≥ ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a = ⊥, b ≥ ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a ≥ ⊥, b = ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a ≥ ⊥, b = ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a = ⊥, b = ⊥), a, b)", "(a = ⊥, b = a)", []);
    ("unify((a = ⊥, b = ⊥), b, a)", "(b = ⊥, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a ≥ ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b ≥ ∀(x, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), a, b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), b, a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀x.x → x), a, b)", "(a ≥ ∀(x2, x = x2).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b ≥ ∀x.x → x), b, a)", "(b ≥ ∀(x2, x = x2).x → x, a = b)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀x.x → x), a, b)", "(a = ∀(x2, x = x2).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b ≥ ∀x.x → x), b, a)", "(b = ∀(x2, x = x2).x → x, a = b)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀x.x → x), a, b)", "(a = ∀(x2, x = x2).x → x, b = a)", []);
    ("unify((a ≥ ∀x.x → x, b = ∀x.x → x), b, a)", "(b = ∀(x2, x = x2).x → x, a = b)", []);
    ("unify((a = ∀x.x → x, b = ∀x.x → x), a, b)", "(a = ∀(x2, x = x2).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀x.x → x), b, a)", "(b = ∀(x2, x = x2).x → x, a = b)", []);
    ("unify((x), x, x)", "(x)", []);
    ("unify((a ≥ number), a, number)", "(a ≥ number)", []);
    ("unify((a = number), a, number)", "(a = number)", []);
    ("unify((a ≥ number), a, string)", "(a ≥ number)", ["number ≢ string"]);
    ("unify((a = number), a, string)", "(a = number)", ["number ≢ string"]);
    ("unify((a ≥ number), number, a)", "(a ≥ number)", []);
    ("unify((a = number), number, a)", "(a = number)", []);
    ("unify((a ≥ number), string, a)", "(a ≥ number)", ["string ≢ number"]);
    ("unify((a = number), string, a)", "(a = number)", ["string ≢ number"]);
    ("unify((a, b = a), a, b)", "(a, b = a)", []);
    ("unify((a, b = a), b, a)", "(a, b = a)", []);
    ("unify((a, b ≥ a), a, b)", "(a, b ≥ a)", []);
    ("unify((a, b ≥ a), b, a)", "(a, b ≥ a)", []);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → string), a, b)", "(a ≥ ∀x.x → number, b ≥ ∀y.y → string)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → string), b, a)", "(a ≥ ∀x.x → number, b ≥ ∀y.y → string)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → string), a, b)", "(a = ∀x.x → number, b ≥ ∀y.y → string)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → string), b, a)", "(a = ∀x.x → number, b ≥ ∀y.y → string)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → string), a, b)", "(a ≥ ∀x.x → number, b = ∀y.y → string)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → string), b, a)", "(a ≥ ∀x.x → number, b = ∀y.y → string)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number, b = ∀y.y → string), a, b)", "(a = ∀x.x → number, b = ∀y.y → string)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number, b = ∀y.y → string), b, a)", "(a = ∀x.x → number, b = ∀y.y → string)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → number), a, b)", "(a ≥ ∀(y, x = y).x → number, b = a)", []);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → number), b, a)", "(b ≥ ∀(x, y = x).y → number, a = b)", []);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → number), a, b)", "(a = ∀(y, x = y).x → number, b = a)", []);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → number), b, a)", "(b = ∀(x, y = x).y → number, a = b)", []);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → number), a, b)", "(a = ∀(y, x = y).x → number, b = a)", []);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → number), b, a)", "(b = ∀(x, y = x).y → number, a = b)", []);
    ("unify((a = ∀x.x → number, b = ∀y.y → number), a, b)", "(a = ∀(y, x = y).x → number, b = a)", []);
    ("unify((a = ∀x.x → number, b = ∀y.y → number), b, a)", "(b = ∀(x, y = x).y → number, a = b)", []);
    ("unify((c, a ≥ ∀x.x → c, b ≥ ∀y.y → a), a, b)", "(c, a ≥ ∀x.x → c, b ≥ ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a ≥ ∀x.x → c, b ≥ ∀y.y → a), b, a)", "(c, a ≥ ∀x.x → c, b ≥ ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a = ∀x.x → c, b ≥ ∀y.y → a), a, b)", "(c, a = ∀x.x → c, b ≥ ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a = ∀x.x → c, b ≥ ∀y.y → a), b, a)", "(c, a = ∀x.x → c, b ≥ ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a ≥ ∀x.x → c, b = ∀y.y → a), a, b)", "(c, a ≥ ∀x.x → c, b = ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a ≥ ∀x.x → c, b = ∀y.y → a), b, a)", "(c, a ≥ ∀x.x → c, b = ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a = ∀x.x → c, b = ∀y.y → a), a, b)", "(c, a = ∀x.x → c, b = ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((c, a = ∀x.x → c, b = ∀y.y → a), b, a)", "(c, a = ∀x.x → c, b = ∀y.y → a)", ["Infinite type since `c` occurs in `∀x.x → c`."]);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → y), a, b)", "(a ≥ ∀(y = number, x = y).x → number, b = a)", []);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → y), b, a)", "(b ≥ ∀(x = number, y = x).y → y, a = b)", []);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → y), a, b)", "(a = ∀x.x → number, b ≥ ∀y.y → y)", ["∀(y = number, x = y).x → number ≢ ∀x.x → number"]);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → y), b, a)", "(a = ∀x.x → number, b ≥ ∀y.y → y)", ["∀(x = number, y = x).y → y ≢ ∀x.x → number"]);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → y), a, b)", "(a ≥ ∀x.x → number, b = ∀y.y → y)", ["∀(y = number, x = y).x → number ≢ ∀y.y → y"]);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → y), b, a)", "(a ≥ ∀x.x → number, b = ∀y.y → y)", ["∀(x = number, y = x).y → y ≢ ∀y.y → y"]);
    ("unify((a = ∀x.x → number, b = ∀y.y → y), a, b)", "(a = ∀x.x → number, b = ∀y.y → y)", ["∀(y = number, x = y).x → number ≢ ∀x.x → number"; "∀(y = number, x = y).x → number ≢ ∀y.y → y"]);
    ("unify((a = ∀x.x → number, b = ∀y.y → y), b, a)", "(a = ∀x.x → number, b = ∀y.y → y)", ["∀(x = number, y = x).y → y ≢ ∀y.y → y"; "∀(x = number, y = x).y → y ≢ ∀x.x → number"]);
    ("unify((a ≥ ∀x.x → number, b), a, b → string)", "(a ≥ ∀x.x → number, b)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number, b), a, b → number)", "(b, a = b → number)", []);
    ("unify((a = ∀x.x → number, b), a, b → string)", "(a = ∀x.x → number, b)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number, b), a, b → number)", "(a = ∀x.x → number, b)", ["b → number ≢ ∀x.x → number"]);
    ("unify((a ≥ ∀x.x → number), a, boolean → string)", "(a ≥ ∀x.x → number)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number), a, boolean → number)", "(a = boolean → number)", []);
    ("unify((a = ∀x.x → number), a, boolean → string)", "(a = ∀x.x → number)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number), a, boolean → number)", "(a = ∀x.x → number)", ["boolean → number ≢ ∀x.x → number"]);
    ("unify((a ≥ ∀x.x → number), a, a → string)", "(a ≥ ∀x.x → number)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number), a, a → number)", "(a ≥ ∀x.x → number)", ["Infinite type since `a` occurs in `a → number`."]);
    ("unify((a = ∀x.x → number), a, a → string)", "(a = ∀x.x → number)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number), a, a → number)", "(a = ∀x.x → number)", ["Infinite type since `a` occurs in `a → number`."]);
    ("unify((a ≥ ∀x.x → number, b), b → string, a)", "(a ≥ ∀x.x → number, b)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number, b), b → number, a)", "(b, a = b → number)", []);
    ("unify((a = ∀x.x → number, b), b → string, a)", "(a = ∀x.x → number, b)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number, b), b → number, a)", "(a = ∀x.x → number, b)", ["b → number ≢ ∀x.x → number"]);
    ("unify((a ≥ ∀x.x → number), boolean → string, a)", "(a ≥ ∀x.x → number)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number), boolean → number, a)", "(a = boolean → number)", []);
    ("unify((a = ∀x.x → number), boolean → string, a)", "(a = ∀x.x → number)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number), boolean → number, a)", "(a = ∀x.x → number)", ["boolean → number ≢ ∀x.x → number"]);
    ("unify((a ≥ ∀x.x → number), a → string, a)", "(a ≥ ∀x.x → number)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number), a → number, a)", "(a ≥ ∀x.x → number)", ["Infinite type since `a` occurs in `a → number`."]);
    ("unify((a = ∀x.x → number), a → string, a)", "(a = ∀x.x → number)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number), a → number, a)", "(a = ∀x.x → number)", ["Infinite type since `a` occurs in `a → number`."]);
    ("unify((a, b = ∀x.x → x), a, b)", "(a = ∀x.x → x, b = a)", []);
    ("unify((a, b = ∀x.x → x), b, a)", "(b = ∀x.x → x, a = b)", []);
    ("unify((a, b ≥ ∀x.x → x), a, b)", "(a ≥ ∀x.x → x, b = a)", []);
    ("unify((a, b ≥ ∀x.x → x), b, a)", "(b ≥ ∀x.x → x, a = b)", []);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → string), a, b)", "(a ≥ ∀x.x → number, b ≥ ∀y.y → string)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → string), b, a)", "(a ≥ ∀x.x → number, b ≥ ∀y.y → string)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → number), a, b)", "(a ≥ ∀(y, x = y).x → number, b = a)", []);
    ("unify((a ≥ ∀x.x → number, b ≥ ∀y.y → number), b, a)", "(b ≥ ∀(x, y = x).y → number, a = b)", []);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → string), a, b)", "(a = ∀x.x → number, b ≥ ∀y.y → string)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → string), b, a)", "(a = ∀x.x → number, b ≥ ∀y.y → string)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → number), a, b)", "(a = ∀(y, x = y).x → number, b = a)", []);
    ("unify((a = ∀x.x → number, b ≥ ∀y.y → number), b, a)", "(b = ∀(x, y = x).y → number, a = b)", []);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → string), a, b)", "(a ≥ ∀x.x → number, b = ∀y.y → string)", ["number ≢ string"]);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → string), b, a)", "(a ≥ ∀x.x → number, b = ∀y.y → string)", ["string ≢ number"]);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → number), a, b)", "(a = ∀(y, x = y).x → number, b = a)", []);
    ("unify((a ≥ ∀x.x → number, b = ∀y.y → number), b, a)", "(b = ∀(x, y = x).y → number, a = b)", []);
    ("unify((a = ∀x.x → number, b = ∀y.y → string), a, b)", "(a = ∀x.x → number, b = ∀y.y → string)", ["number ≢ string"]);
    ("unify((a = ∀x.x → number, b = ∀y.y → string), b, a)", "(a = ∀x.x → number, b = ∀y.y → string)", ["string ≢ number"]);
    ("unify((a = ∀x.x → number, b = ∀y.y → number), a, b)", "(a = ∀(y, x = y).x → number, b = a)", []);
    ("unify((a = ∀x.x → number, b = ∀y.y → number), b, a)", "(b = ∀(x, y = x).y → number, a = b)", []);
    ("unify((a = ∀x.x → x, b = ∀x.x → x), a, b)", "(a = ∀(x2, x = x2).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀x.x → x), b, a)", "(b = ∀(x2, x = x2).x → x, a = b)", []);
    ("unify((a, b = a, c = b, d = c, e = d, f = e → e), a, f)", "(a, b = a, c = b, d = c, e = d, f = e → e)", ["Infinite type since `a` occurs in `e → e`."]);
    ("unify((a, b = a, c = b, d = c, e = d, f = e → e), f, a)", "(a, b = a, c = b, d = c, e = d, f = e → e)", ["Infinite type since `a` occurs in `e → e`."]);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), a → a, a → b)", "(a = ∀(y, x = y).x → x, b = a)", []);
    ("unify((a = ∀x.x → x, b = ∀y.y → y), a → b, a → a)", "(b = ∀(x, y = x).y → y, a = b)", []);
    ("unify((t = ∀(a = ∀x.x → x).a → a, u = ∀(b = ∀y.y → y, c = ∀z.z → z).b → c), t, u)", "(t = ∀(b = ∀(z, x = z, y = x).y → y, a = b).a → a, u = t)", []);
    ("unify((t = ∀(a = ∀x.x → x).a → a, u = ∀(b = ∀y.y → y, c = ∀z.z → z).b → c), u, t)", "(u = ∀(c = ∀(y, z = y).z → z, a = c, b = a).b → c, t = u)", []);
    ("unify((a = ∀(x, x = ∀z.z → x).x → x, b), a, b)", "(a = ∀(x, x = ∀z.z → x).x → x, b = a)", []);
    ("unify((a = ∀(x, x = ∀z.z → x).x → x, b), b, a)", "(b = ∀(x, x = ∀z.z → x).x → x, a = b)", []);
    ("unify((a = ∀(x, x = ∀z.z → x).x → x, b, c, d, e), a, (b → c) → d → e)", "(a = ∀(x, x = ∀z.z → x).x → x, c, e = c, d, b)", ["b → c ≢ ∀z.z → x"; "d → e ≢ ∀z.z → x"]);
    ("unify((a = ∀(x, x = ∀z.z → x).x → x, b, c, d, e), (b → c) → d → e, a)", "(a = ∀(x, x = ∀z.z → x).x → x, e, d, b, c = e)", ["b → c ≢ ∀z.z → x"; "d → e ≢ ∀z.z → x"]);
    ("unify((a ≥ ∀(x, x ≥ ∀z.z → x).x → x, b, c, d, e), a, (b → c) → d → e)", "(d, b = d, c, e = c, a = (b → c) → d → e)", []);
    ("unify((a ≥ ∀(x, x ≥ ∀z.z → x).x → x, b, c, d, e), (b → c) → d → e, a)", "(b, e, c = e, d = b, a = (b → c) → d → e)", []);
    ("unify((a = ∀(x, x = ∀z.z → x).x → x, b = ∀(x, x = ∀z.z → x).x → x), a, b)", "(a = ∀(x3, x4 = ∀(z2, z = z2).z → x3, x2 = x4).x2 → x2, b = a)", []);
    ("unify((a = ∀(x, x2, x = ∀z.z → x2 → x).x → x, b = ∀(x, x2, x = ∀z.z → x2 → x).x → x), a, b)", "(a = ∀(x4, x2, x5 = x2, x6 = ∀(z2, z = z2).z → x5 → x4, x3 = x6).x3 → x3, b = a)", []);
    ("unify((a = ∀(x1, x1 = ∀z.z → x1).x1 → x1, b = ∀(x1, x1 = ∀z.z → x1).x1 → x1), a, b)", "(a = ∀(x3, x4 = ∀(z2, z = z2).z → x3, x2 = x4).x2 → x2, b = a)", []);
    ("unify((a = ∀(x1, x2, x1 = ∀z.z → x2 → x1).x1 → x1, b = ∀(x1, x2, x1 = ∀z.z → x2 → x1).x1 → x1), a, b)", "(a = ∀(x4, x2, x5 = x2, x6 = ∀(z2, z = z2).z → x5 → x4, x3 = x6).x3 → x3, b = a)", []);
    ("unify((a = ∀(x5, x5 = ∀z.z → x5).x5 → x5, b = ∀(x5, x5 = ∀z.z → x5).x5 → x5), a, b)", "(a = ∀(x7, x8 = ∀(z2, z = z2).z → x7, x6 = x8).x6 → x6, b = a)", []);
  ] in

  let prefix = Prefix.create () in

  cases |> List.iter (fun (input, output, expected_errors) -> (
    let name = match List.length expected_errors with
    | 0 -> Printf.sprintf "%s = %s" input output
    | 1 -> Printf.sprintf "%s = %s with 1 error" input output
    | n -> Printf.sprintf "%s = %s with %n errors" input output n
    in
    test name (fun () -> (
      let (result, actual_errors) = Diagnostic.collect (fun () -> Prefix.level prefix (fun () -> (
        let tokens = Parser.tokenize (Stream.of_string input) in
        assert (Stream.next tokens = Identifier "unify");
        assert (Stream.next tokens = Glyph ParenthesesLeft);
        let _ = Parser.parse_prefix tokens (fun (name, bound) -> assert (Prefix.add prefix name bound = None)) in
        assert (Stream.next tokens = Glyph Comma);
        let actual = Parser.parse_monotype tokens in
        assert (Stream.next tokens = Glyph Comma);
        let expected = Parser.parse_monotype tokens in
        assert (Stream.next tokens = Glyph ParenthesesRight);
        Stream.empty tokens;
        let result = Unify.unify prefix actual expected in
        assert_equal (Printer.print_prefix prefix) output;
        result
      ))) in
      assert (match result with Ok () -> actual_errors = [] | Error _ -> actual_errors <> []);
      List.iter2 assert_equal (List.map Printer.print_diagnostic actual_errors) expected_errors
    ))
  ))
))
