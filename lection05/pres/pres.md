# Коллекции и контейнеры данных

---

# Обо мне
- Владислав Бондаренко
- Работаю в Tinkoff с 2019 года
- Разрабатываю на Scala 5 лет
- До сих пор делаю для себя открытия
- Давайте общаться на "ты"

---

## Где мы сейчас?

![[current.svg]]

---

## План лекции
1. Кратко о дженериках (generics)
2. Вариантность
3. Терминология
4. Option
5. Try
6. Either
7. Коллекции

---

# Дженерики

---

```scala
def unsafeHead(l: List[Int]): Int = l(0)
def unsafeHead(l: List[String]): String = l(0)
def unsafeHead(l: List[Boolean]): Boolean = l(0)
```

---

```scala
def unsafeHead[A](l: List[A]): A = l(0)
```

---

```scala
case class Foo[A](a: A)
case class Bar[A, B](a: A, b: B)
case class Baz[A, B, C](a: A, b: B, c: C)
```

---

```scala
trait Foo[A] {
  def foo(): A
}

trait Bar[A, B] {
  def bar(a: A): B
}
```

---

# Вариантность

---

***Вариантность*** - способ переноса схемы наследования для производных типов

---

```scala
trait Foo[A]  //инвариантый параметр
trait Bar[+A] //ковариантный параметр
trait Baz[-A] //контравариатный параметр
```

---

```scala
                               //            ┌───┐                             
trait A                        //            │ A │                             
                               //            └─▲─┘                              
                               //              │                             
                               //            ┌─┴─┐                              
trait B extends A              //            │ B │                             
                               //            └───┘                             
```

---

## Инвариантность
_Схема наследования не переносится_

```scala
trait Foo[T]                    //         ┌────────┐
                                //         │ Foo[A] │
trait A                         //         └────────┘
trait B extends A               //
                                //
                                //         ┌────────┐
val fa = new Foo[A] {}          //         │ Foo[B] │
val fb = new Foo[B] {}          //         └────────┘
```

---

## Ковариантность

_Схема наследования переносится как есть_

```scala
trait Foo[+T]                   //         ┌────────┐
                                //         │ Foo[A] │
trait A                         //         └────▲───┘
trait B extends A               //             ┌┘
                                //         ┌───┴────┐
val fa = new Foo[A] {}          //         │ Foo[B] │
val fb = new Foo[B] {}          //         └────────┘
```

---

## Контравариантность

_Схема наследования инвертируется_

```scala
trait Foo[-T]                   //      ┌────────┐
                                //      │ Foo[A] │
trait A                         //      └───┬────┘
trait B extends A               //          └───────────┐
                                //                ┌─────▼──┐
val fa = new Foo[A] {}          //                │ Foo[B] │
val fb = new Foo[B] {}          //                └────────┘
```

---

### Контринтуитивная контравариантность

```scala
class Fruit
class Apple     extends Fruit
class Antonovka extends Apple 
```

```scala
trait Show[-T] {
  def show(v: T): String
}
```

---

```scala
val showFruit: Show[Fruit] = new Show[Fruit] {
  def show(f: Fruit): String = "Фрукт"
}

val showAntonovka: Show[Antonovka] = showFruit

val apple = new Antonovka

println(showAntonovka.show(apple))

//>>> Фрукт
```

---

## Терминология

- _**Тип**_ - данные характеризуемые набором каких-либо аттрибутов
- _**Конструктор типа**_ - "*тип*" параметризующийся другим типом/типами
- _**Параметрический полиморфизм**_ - один код для разных типов
- _**Специальный (ad-hoc) полиморфизм**_ - разный код для разных типов по обращению к одной и той же функции (*перегрузка функции*)

---

# Контейнеры и коллекции

---

# Option

---

***Option*** - замена ***null*** и явно говорит о том, присутствует значение или его нет
![[option.svg]]

---

```scala
sealed abstract class Option[+T]
case class Some[+A](v: A) extends Option[A]
case object None extends Option[Nothing]
```

---

# Try

---

***Try*** - один из способов работы с ошибками в Scala, обычно, используется при взаимодействии с java кодом
![[try.svg]]
 
---

```scala
sealed abstract class Try[+T]
case class Success[+T](value: T) extends Try[T]
case class Failure[+T](thr: Throwable) extends Try[T]
```

---

# Either

---

***Either*** - когда Option нам недостаточно, и мы хотим указать причину отсутствия значения и/или вернуть другую полезну информацию, которая может помочь клиентскому коду

![[either.svg]]

---

```scala
sealed abstract class Either[+A, +B]
case class Left[+A, +B](v: A) extends Either[A, B]
case class Right[+A, +B](v: B) extends Either[A, B]
```

---

![[collections.svg]]

---

# Полезные ссылки

- [Официальная документация](https://docs.scala-lang.org/overviews/collections-2.13/introduction.html)
- [Перформанс коллекций](https://docs.scala-lang.org/overviews/collections-2.13/performance-characteristics.html)
  
---

# Спасибо за внимание

