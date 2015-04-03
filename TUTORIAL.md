# Kits

kitsはScalaの関数プログラミングを支援するライブラリである.

このライブラリを通してScalaに標準で存在しない抽象概念について紹介する.

## Monoid

モノイドは単位元と結合演算をもつ.

モノイドを型クラスとして表現すると次のようになる.

```scala
trait Monoid[A] {
  def empty: A
  def append(x: A, y: A): A
}
```

`Int`は単位元が`0`, 結合演算が`+`のモノイドである.

```scala
implicit val intMonoid = new Monoid[Int] {
  def empty: Int = 0
  def append(x: Int, y: Int): Int = x + y
}
```

型クラスのインスタンスは`implicit value`として定義する.

`implicit value`は`val`, `object`, `def`により定義できる.

特に`def`は`type parameter`と`implicit parameter`をとることができる.

次に`Monoid`を使った関数を定義する.

```scala
def double[A](a: A)(implicit A: Monoid[A]): A = A.append(a, a)

assert(double(2) == 4)
assert(double(3) == 6)
```

関数は型クラスを`implicit parameter`としてとることでインスタンスを明示する必要がなくなる.

`String`は単位元が空文字列, 結合演算が文字列連結のモノイドである.

```scala
implicit val stringMonoid = new Monoid[String] {
  def empty: String = ""
  def append(x: String, y: String): String = x + y
}

assert(double("hoge") == "hogehoge")
assert(double("fuga") == "fugafuga")
```

このように, 型クラスは既存の型に対して抽象化が可能であり, 振る舞いを付け加えることができる.

kitsではモノイドが`kits.Monoid`に定義され, そのインスタンスはコンパニオンオブジェクトに定義される.

```scala
assert(kits.Monoid.append("foo", "bar", "baz") == "foobarbaz")

assert(kits.Monoid.multiply("hoge", 3) == "hogehogehoge")
```

`implicit value`は現在のスコープ以外にもデータ型や型クラスのコンパニオンオブジェクトから探索される.

数値のインスタンスは2通り実装されており`import`により実装を選択する.

```scala
{
  import kits.Monoid.sum
  assert(kits.Monoid.append(2, 3) == 5)
}

{
  import kits.Monoid.product
  assert(kits.Monoid.append(2, 3) == 6)
}
```

## Applicative

最初に`Functor`の定義を示す.

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

これは`List`や`Option`, `Future`などがもつ`map`に関して抽象化した型クラスである.

そして`Applicative`は`Functor`を継承する.

```scala
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))
}
```

* `pure`は値を`F`に写す.
* `apply`は`F`の文脈で引数を順次評価し関数を適用する.

これらは任意の関数を`F`の文脈で適用することを可能にする.

```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] =
  F.apply(fb)(F.map(fa)(a => f(a, _)))

def map3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] =
  F.apply(fc)(map2(fa, fb)((a, b) => f(a, b, _)))
```

多くの場合に`Applicative`は`Monad`により定義される.

```scala
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
}
```

`List`や`Option`はモナドであり, インスタンスは次のように定義できる.

```scala
implicit val listMonad = new Monad[List] {
  def pure[A](a: A): List[A] = a :: Nil
  def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
}

implicit val optionMonad = new Monad[Option] {
  def pure[A](a: A): Option[A] = Some(a)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
}
```

`Applicative`は`Monad`より軽量な記述を可能にする.

```scala
case class User(id: Int, name: String)

assert((for (id <- Option(346); name <- Option("halcat")) yield User(id, name)) == Some(User(346, "halcat")))

assert(map2(Option(346), Option("halcat"))(User) == Some(User(346, "halcat")))
```

kitsでは`kits.Functor`, `kits.Applicative`, `kits.Monad`が定義され, それらのインスタンスは`kits.Functor`に定義される.

これは`implicit value`の探索がスーパークラスのコンパニオンオブジェクトに対しても行われるためである.

`kits.Applicative`は次のように使える.

```scala
assert(kits.Applicative.map(List(1, 2), List(3))(_ + _) == List(4, 5))

type Result[A] = Either[String, A]

{
  import kits.Functor.right
  assert(kits.Applicative.map(Left("foo"): Result[Int], Right(1): Result[Int], Left("bar"): Result[Int])(_ + _ + _) == Left("foo"))
}

{
  import kits.Applicative.right
  assert(kits.Applicative.map(Left("foo"): Result[Int], Right(1): Result[Int], Left("bar"): Result[Int])(_ + _ + _) == Left("foobar"))
}
```

Scalaは高階型の単一化が弱いので`Either`などには別名を付ける必要がある.

`Either`には2つの`Applicative`が定義される.

`kits.Functor.right`は最初の`Left`の値を返し, `kits.Applicative.right`は`Left`の値を`Monoid`により結合する.

## Traverse

`Traverse`の定義を次に示す.

```scala
trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)
}
```

`traverse`は`fa`の各要素を`f`に適用し結果を`G`の文脈で`F`に収集する.

`Identity`の文脈で`traverse`は`map`と同じ結果が得られる.

```scala
type Identity[A] = A

implicit val identityApplicative = new Applicative[Identity] {
  def pure[A](a: A): Identity[A] = a
  def apply[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = f(fa)
}
```

`Identity`は自身に写すため, `Applicative`は単なる関数適用として作用する.

`List`に対する`Traverse`のインスタンスは次のようになる.

```scala
implicit val listTraverse = new Traverse[List] {
  def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
    fa.foldRight(F.pure(Nil: List[B]))((a, ga) => F(ga)(F.map(f(a))(b => b :: _)))
}
```

`Traverse`はいくつかの有用なたたみ込み関数を提供する.

`sequence`はデータ構造と計算コンテナを入れ替える関数である.

```scala
def sequence[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F], G: Applicative[G]): G[F[A]] = F.traverse(fga)(identity)

assert(sequence(List(Option(1), Option(2), Option(3))) == Some(List(1, 2, 3)))

assert(sequence(List(Some(1), None, Some(3))) == None)
```

`foldMap`は`Monoid`を使って`Traverse`を畳み込む関数である.

定義には`Monoid`からなる`Applicative`を利用する.

```scala
def monoidApplicative[A](A: Monoid[A]) = new Applicative[({ type F[B] = A })#F] {
  def pure[B](b: B) = A.empty
  def apply[B, C](fa: A)(f: A) = A.append(f, fa)
}

def foldMap[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Traverse[F], B: Monoid[B]): B = F.traverse[({ type G[A] = B })#G, A, B](fa)(f)(monoidApplicative(B))

assert(foldMap(List("hello", "world"))(identity) == "helloworld")

{
  import kits.Monoid.sum
  assert(foldMap(List("hello", "world"))(_.size) == 10)
}
```
