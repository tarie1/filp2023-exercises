package exercises02

object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    text.toLowerCase
      .split("[^a-zа-яё'-]")
      .filterNot(_ == "")
      .foldLeft(Map.empty[String, Int]) { (count, word) =>
        count + (word -> (count.getOrElse(word, 0) + 1))
      }
  }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = {
    text.toLowerCase
      .split("[^a-z'-]")
      .filterNot(_ == "")
      .foldLeft(Map.empty[String, Int]) { (count, word) =>
        count + (word -> (count.getOrElse(word, 0) + 1))
      }
  }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = {
    text.toLowerCase
      .split("[,. ]*[a-zа-я-!?:\\s]")
      .filterNot(_ == "")
      .foldLeft(Map.empty[String, Int]) { (count, word) =>
        count + (word -> (count.getOrElse(word, 0) + 1))
      }
  }

}
