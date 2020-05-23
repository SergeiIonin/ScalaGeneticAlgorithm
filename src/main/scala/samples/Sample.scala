package samples

trait Sample[Gene] {

  type Sample = List[Gene]

  type Population = List[Sample]
}

