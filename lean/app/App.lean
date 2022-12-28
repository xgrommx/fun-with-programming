-- def f (a: Nat) (b: Nat) := a + b

-- namespace Maybe

-- universe u

inductive Maybe (α : Type u) where
  | nothing : Maybe α
  | just (val : α) : Maybe α

-- constant Fix (f : Type → Type) : Type

-- instance : Inhabited (Fix f) where
--   default := sorry

-- unsafe def projectUnsafe : Fix f → f (Fix f) := unsafeCast

-- abbrev Algebra (f : Type → Type) (α : Type) := f α → α

-- abbrev Coalgebra (f : Type → Type) (α : Type) := α → f α

-- @[implementedBy projectUnsafe]
-- constant project : Fix f → f (Fix f) := sorry

-- prefix:max "↓" => project

-- unsafe def embedUnsafe : f (Fix f) → Fix f := unsafeCast

-- @[implementedBy embedUnsafe]
-- constant embed : f (Fix f) → Fix f := sorry

-- prefix:max "↑" => embed

-- partial def cata [Functor f] [Inhabited α] (algebra : Algebra f α) (fix : Fix f) : α :=
--   algebra (cata algebra <$> ↓fix)

-- partial def ana [Functor f] (coalgebra : Coalgebra f α) (a : α) : Fix f :=
--   ↑(ana coalgebra <$> coalgebra a)

-- inductive ExpF (α : Type) where
--   | nat : Nat → ExpF α
--   | add : α → α → ExpF α
--   | mul : α → α → ExpF α

-- instance : Functor ExpF where
--   map f
--     | ExpF.nat n₁    => ExpF.nat n₁
--     | ExpF.add a₁ a₂ => ExpF.add (f a₁) (f a₂)
--     | ExpF.mul a₁ a₂ => ExpF.mul (f a₁) (f a₂)

-- abbrev Exp := Fix ExpF

-- #eval
--   let eval : Algebra ExpF Nat
--     | ExpF.nat n₁    => n₁
--     | ExpF.add a₁ a₂ => a₁ + a₂
--     | ExpF.mul a₁ a₂ => a₁ * a₂
--   let exp : Exp := ↑(ExpF.mul ↑(ExpF.add ↑(ExpF.nat 1) ↑(ExpF.nat 2)) ↑(ExpF.nat 3))
--   cata eval exp

-- inductive Fix (f : Type -> Type) : Type where
--   | fix : f (Fix f) -> Fix f

-- attribute [unbox] Maybe

abbrev Algebra (f : Type u → Type u) (α : Type u) := f α → α
abbrev Coalgebra (f : Type u → Type u) (α : Type u) := α → f α

namespace Maybe

def map {α β : Type u} (f : α -> β) : Maybe α -> Maybe β
  | nothing => nothing
  | just a => just (f a)

instance : Functor Maybe where
  map := map

end Maybe

-- end Maybe

def zip : List α -> List β -> List (Prod α β)
  | x::xs, y::ys => (x, y) :: zip xs ys
  | _, _ => []

def alternate : List α -> List α -> List α
  | [], as => as
  | bs, [] => bs
  | x::xs, y::ys => x :: y :: alternate xs ys

unsafe inductive Fix (f : Type u → Type u)
| mk : f (Fix f) → Fix f

class Recursive (t : Type u) (f : outParam (Type u → Type u)) extends Functor f where
  project : Coalgebra f t
  -- cata : (f a → a) → t → a

unsafe def cata [Recursive t f] (alg : Algebra f α) : t → α :=
  alg ∘ Functor.map (cata alg) ∘ Recursive.project

unsafe def Fix.project : Fix f → (f (Fix f))
  | mk f => f

-- alg (map (cata alg) (project t))

unsafe def Fix.cata [Functor f] (alg : Algebra f a) : Fix f → a
  | Fix.mk f => alg (cata alg <$> f)

-- def map (f : α -> β) : Maybe α -> Maybe β
--   | nothing => nothing
--   | just a => just (f a)

unsafe instance [Functor f] : Recursive (Fix f) f where
  project := Fix.project

inductive ListF (α : Type u) (β : Type u) where
  | nilF : ListF α β
  | consF (x : α) (y : β) : ListF α β

namespace ListF

def map (f : α -> β) : ListF γ α -> ListF γ β
  | nilF => nilF
  | consF a b => consF a (f b)

def project : Coalgebra (ListF a) (List a)
  | [] => ListF.nilF
  | List.cons a b => ListF.consF a b

-- def cata (alg : ListF a a -> a) : List a -> a
--   | [] -> nilF
--   | L.cons a b ->   

end ListF

-- unsafe def Fix.cata [Functor f] (alg : f a → a) : Fix f → a
--   | Fix.mk f => alg (cata alg <$> f)

instance : Functor (ListF a) where
  map := ListF.map

instance : Recursive (List a) (ListF a) where
  project := ListF.project

def sumAlg : Algebra (ListF Int) Int
  | ListF.nilF => 0
  | ListF.consF a b => a + b

unsafe def sum : List Int -> Int := cata sumAlg

#check @cata
#check sumAlg
#eval sum [1,2,3,4,5]
#eval zip [1,2,3] [4,5,6]