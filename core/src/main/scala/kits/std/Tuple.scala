package kits

package std

trait Tuple2Monoid[A, B] extends Monoid[(A, B)] {

  def _1: Monoid[A]

  def _2: Monoid[B]

  override final def empty: (A, B) = (_1.empty, _2.empty)

  override final def append(x: (A, B), y: (A, B)): (A, B) =
    (x, y) match {
      case ((ax, bx), (ay, by)) => (_1.append(ax, ay), _2.append(bx, by))
    }

}

trait Tuple3Monoid[A, B, C] extends Monoid[(A, B, C)] {

  def _1: Monoid[A]

  def _2: Monoid[B]

  def _3: Monoid[C]

  override final def empty: (A, B, C) = (_1.empty, _2.empty, _3.empty)

  override final def append(x: (A, B, C), y: (A, B, C)): (A, B, C) =
    (x, y) match {
      case ((ax, bx, cx), (ay, by, cy)) => (_1.append(ax, ay), _2.append(bx, by), _3.append(cx, cy))
    }

}
