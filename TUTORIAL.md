# Kits

kitsはScalaの関数プログラミングを支援するライブラリである.

このライブラリを通してScalaには標準で存在しない抽象概念について学ぶ.

## Monoid

モノイドは単位元と結合演算をもつ.

モノイドを型クラスとして表現すると次のようになる.

```scala
trait Monoid[A] {
  def zero: A
  def append(x: A, y: A): A
}
```

`Int`は単位元が`0`, 結合演算が`+`のモノイドである.

```scala
implicit val intMonoid = new Monoid[Int] {
  def zero: Int = 0
  def append(x: Int, y: Int): Int = x + y
}
```

型クラスのインスタンスは`implicit value`として定義する.

`implicit value`は`val`, `object`, `def`により定義できる.

特に`def`は`type parameter`と`implicit parameter`をとることができる.

`Monoid`を使った関数を定義する.

```scala
def double[A](a: A)(implicit A: Monoid[A]): A = A.append(a, a)

assert(double(2) == 4)
assert(double(3) == 6)
```

関数は型クラスを`implicit parameter`としてとることでインスタンスを明示する必要がなくなる.

`String`に対する`Monoid`のインスタンスを定義する.

```scala
implicit val stringMonoid = new Monoid[String] {
  def zero: String = ""
  def append(x: String, y: String): String = x + y
}

assert(double("hoge") == "hogehoge")
assert(double("fuga") == "fugafuga")
```

このように, 型クラスは既存の型に対して抽象化が可能であり, 振る舞いを付け加えることができる.

kitsではモノイドが`kits.Monoid`に定義され, そのインスタンスはコンパニオンオブジェクトに定義される.

```scala
def double[A](a: A)(implicit A: kits.Monoid[A]): A = A.append(a, a)

assert(double("hoge") == "hogehoge")
assert(double(List("hoge")) == List("hoge", "hoge"))
```

`implicit value`は現在のスコープ以外にもデータ型や型クラスのコンパニオンオブジェクトから探索される.

Intのインスタンスは2通り実装されており`import`により実装を選択する.

```scala
{
  import kits.Monoid.sum
  assert(double(3) == 6)
}

{
  import kits.Monoid.product
  assert(double(3) == 9)
}
```

モノイドの応用は`Traverse`に関する解説で紹介する.

## Applicative

`Functor`は関数を`F`に写し適用する`map`をもつ.

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

これは`List`や`Option`, `Future`などがもつ`map`に関して抽象化した型クラスである.

`Applicative`は`Functor`を継承する.

```scala
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))
}
```

* `pure`は値を`F`に写す.
* `apply`は`F`について順次評価し関数を適用する.

`Applicative`は任意の関数を`F`の文脈で適用することを可能にする.

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
assert(kits.Applicative.map(Right(2): Result[Int], Right(3): Result[Int], Left("hoge"): Result[Int])(_ + _ + _) == Left("hoge"))
```

## Traverse
