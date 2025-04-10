package lexer

class Utils

def unionWith[K, V](
    union: (V, V) => V,
    map1: Map[K, V],
    map2: Map[K, V]
): Map[K, V] = {
  ((map1.keySet ++ map2.keys) map { (i: K) =>
    i -> ((map1.get(i), map2.get(i)) match {
      case (Some(v1), Some(v2)) => union(v1, v2)
      case (Some(v1), _)        => v1
      case (_, Some(v2))        => v2
    })
  }).toMap
}
