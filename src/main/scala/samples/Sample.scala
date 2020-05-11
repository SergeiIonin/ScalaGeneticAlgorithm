package samples

trait Sample[Gene] {

  type Sample[Gene] = List[Gene]

  type Population[Gene] = List[Sample[Gene]]
}

