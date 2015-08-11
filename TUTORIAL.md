# Kits

kitsはScalaの関数プログラミングを支援するライブラリです.

このライブラリを通してScalaに標準で存在しない抽象概念について紹介します.

## Monoid

モノイドは単位元と結合演算をもちます.

モノイドを型クラスとして表現すると次のようになります.

```scala
trait Monoid[A] {
  def empty: A
  def append(x: A, y: A): A
}
```

`Int`は単位元が`0`, 結合演算が`+`のモノイドです.

```scala
implicit val intMonoid: Monoid[Int] =
  new Monoid[Int] {
    def empty: Int = 0
    def append(x: Int, y: Int): Int = x + y
  }
```

型クラスのインスタンスは`implicit value`として定義します.

`implicit value`は`val`, `object`, `def`により定義できます.

特に`def`は`type parameter`と`implicit parameter`をとることができます.

次にモノイドを使った関数を定義します.

```scala
def double[A](a: A)(implicit A: Monoid[A]): A = A.append(a, a)

assert(double(2) == 4)
assert(double(3) == 6)
```

関数は型クラスを`implicit parameter`にとることでインスタンスを明示する必要がなくなります.

`String`は単位元が空文字列, 結合演算が文字列連結のモノイドです.

```scala
implicit val stringMonoid: Monoid[String] =
  new Monoid[String] {
    def empty: String = ""
    def append(x: String, y: String): String = x + y
  }

assert(double("hoge") == "hogehoge")
assert(double("fuga") == "fugafuga")
```

このように, 型クラスは既存の型に対して抽象化が可能であり, 振る舞いを付け加えることができます.

kitsではモノイドが`kits.Monoid`に定義され, そのインスタンスはコンパニオンオブジェクトに定義されます.

```scala
assert(kits.Monoid.append(List(0, 1), List(2, 3)) == List(0, 1, 2, 3))

assert(kits.Monoid.append("foo", "bar", "baz") == "foobarbaz")
```

`implicit value`は呼び出しのスコープ以外にもデータ型や型クラスのコンパニオンオブジェクトから探索されます.

数値には2種類のインスタンスが実装されており, `Sum`と`Prod`でラップすることで利用できます.

```scala
assert(kits.Monoid.append(kits.Sum(2), kits.Sum(3)) == kits.Sum(5))

assert(kits.Monoid.append(kits.Prod(2), kits.Prod(3)) == kits.Prod(6))
```

半群に単位元を加えることでモノイドをなす例として`Option`や`Map`が存在します.

これらはコンテナ同士の結合にその値のモノイドを利用します.

```scala
assert(kits.Monoid.append(Some("foo"), None, Some("bar")) == Some("foobar"))

assert(kits.Monoid.append(Map('a -> "foo", 'b -> "bar"), Map('a -> "bar", 'c -> "baz")) == Map('a -> "foobar", 'b -> "bar", 'c -> "baz"))
```

## Applicative

ファンクタは`map`に関して抽象化した型クラスです.

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

`map`は関数`A => B`を`F[A] => F[B]`に持ち上げます.

モナドは`flatMap`に関して抽象化した型クラスです.

```scala
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
}
```

`Option`のモナドのインスタンスは次のように定義されます.

```scala
implicit val optionMonad: Monad[Option] =
  new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }
```

アプリカティブはファンクタとモナドの間にある型クラスです.

```scala
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}
```

* `pure`は値をコンテナに包みます.
* `ap`はコンテナに包まれた値にコンテナに包まれた関数を適用します.

これらは任意のアリティの関数を持ち上げることを可能にします.

```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] =
  F.ap(fb)(F.map(fa)(a => f(a, _)))

def map3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] =
  F.ap(fc)(map2(fa, fb)((a, b) => f(a, b, _)))
```

アプリカティブはモナドより軽量な記述を可能にします.

```scala
case class User(id: Int, name: String)

assert(map2(Option(346), Option("halcat"))(User) == Some(User(346, "halcat")))

assert((for (id <- Option(346); name <- Option("halcat")) yield User(id, name)) == Some(User(346, "halcat")))
```

kitsでは`kits.Functor`, `kits.Applicative`, `kits.Monad`が定義され, それらのインスタンスは`kits.Functor`に定義されます.

これは`implicit value`の探索がスーパークラスのコンパニオンオブジェクトに対しても行われるためです.

`kits.Applicative`は次のように使用できます.

```scala
assert(kits.Applicative.map(List(1, 2), List(3))(_ + _) == List(4, 5))

assert(kits.Applicative.map(Some("foo"), None, Some("bar"))(_ + _ + _) == None)
```

`kits.Application.Validation`はエラーを蓄積可能な計算を提供します.

```scala
type Result[A] = kits.Validation[List[String], A]

def fail[A](s: String): Result[A] = kits.Validation(Left(List(s)))

def succeed[A](a: A): Result[A] = kits.Validation(Right(a))

assert(kits.Applicative.map(fail[Int]("foo"), succeed(1), fail[Int]("bar"))(_ + _ + _) == Validation(Left(List("foo", "bar"))))
```

## Traverse

`Traverse`はデータ構造の走査を抽象化した型クラスです.

```scala
trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)
}
```

`traverse`は各要素に関数を適用しアプリカティブのコンテキストで結果を収集します.

`Identity`の文脈で`traverse`は`map`と同じ結果が得られます.

```scala
type Identity[A] = A

implicit val identityApplicative: Applicative[Identity] =
  new Applicative[Identity] {
    def pure[A](a: A): Identity[A] = a
    def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = f(fa)
  }
```

`Identity`は自身に写すため, `Applicative`は単なる関数適用として作用します.

`List`に対する`Traverse`のインスタンスは次のように定義されます.

```scala
implicit val listTraverse: Traverse[List] =
  new Traverse[List] {
    def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      fa.foldRight(F.pure(Nil: List[B]))((a, ga) => F.ap(ga)(F.map(f(a))(b => b :: _)))
  }
```

`Traverse`はいくつかの有用な畳み込み関数を提供します.

`sequence`はデータ構造と計算コンテナを入れ替える関数です.

```scala
def sequence[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F], G: Applicative[G]): G[F[A]] = F.traverse(fga)(identity)

assert(sequence(List(Option(1), Option(2), Option(3))) == Some(List(1, 2, 3)))

assert(sequence(List(Some(1), None, Some(3))) == None)
```

`foldMap`は`Monoid`を使って`Traverse`を畳み込む関数です.

定義には`Monoid`からなる`Applicative`を利用します.

これは`empty`を`pure`に, `append`に`ap`を対応させたものです.

```scala
def monoidApplicative[A](A: Monoid[A]): Applicative[({ type F[B] = A })#F] =
  new Applicative[({ type F[B] = A })#F] {
    def pure[B](b: B) = A.empty
    def ap[B, C](fa: A)(f: A) = A.append(f, fa)
  }

def foldMap[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Traverse[F], B: Monoid[B]): B = F.traverse[({ type G[A] = B })#G, A, B](fa)(f)(monoidApplicative(B))

assert(foldMap(List("hello", "world"))(identity) == "helloworld")

assert(foldMap(List("hello", "world"))(x => Vector(x)) == Vector("hello", "world"))
```
