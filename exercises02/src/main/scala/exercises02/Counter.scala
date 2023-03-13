package exercises02

import scala.util.matching.Regex

object Counter {

  val notEng   = "[^a-z'-]".r
  val notNum   = "[,. ]*[a-zа-я-!?:\\s]".r
  val notWords = "[^a-zа-яё'-]".r

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = {
    notWords
      .split(text.toLowerCase)
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
    notEng
      .split(text.toLowerCase)
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
    notNum
      .split(text.toLowerCase)
      .filterNot(_ == "")
      .foldLeft(Map.empty[String, Int]) { (count, word) =>
        count + (word -> (count.getOrElse(word, 0) + 1))
      }
  }

}
