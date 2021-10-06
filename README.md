# Language in Progress

A language that I don't currently have a name for, however it should focus on these things

* Constructive structural typing
* Integrating dependent types in an imperative language
* Implicit coercion that can be altered by defining proofs for conversion
* An embedding defines a coercion
* Type erasure at runtime

## Examples
#### Syntax is not confirmed

### Embeddings to allow structual typing
```
// defined in stdlib
embedding  ∀ (R r : Row) -> Record R <- Record (R | r)

type Lol = {a : String, b : Int}



fn take_lol(b : Lol) {
  print(b.a);
}

fn main() {
  let b = { a : "hi", b: 1, c: 1.0};
  take_lol(b);
 
}
```
### Conversion from a enum to a number
```
type Days = enum { Mon,Tue,Wed,Thur,Friday, Sat,Sun}

embedding  Nat <- Days = \case 
  Mon -> 0
  Tue -> 1
  Wed -> 2
  Thur -> 3
  Fri -> 4
  Sat -> 5
  Sun -> 6


fn main() {
  println("The number %d",Mon); //prints 0 
} 
```