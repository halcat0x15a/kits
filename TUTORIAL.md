# Kits

`kits`はScalaにおける関数プログラミングを便利にするためのライブラリである.

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

## Traverse
