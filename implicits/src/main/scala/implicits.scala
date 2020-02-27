import java.nio.file._
import java.time.LocalDate


object PathImplicits {

	implicit class fstPath(p: Path) {
    def /(m: String): Path = {
      p.resolve(m)
    }

    def /(m: Path): Path = {
      p.resolve(m)
    }
  }

  implicit class sndPath(p: String) {
    def /(n: String): Path = {
      Paths.get(p).resolve(n)
    }

    def /(n: Path): Path = {
      Paths.get(p).resolve(n)
    }
  }

  implicit class Pwrite(p: Path) {
    def write(m: String) = Files.write(p, m.getBytes)
  }

  implicit class Pread(p: Path) {
    def read(): String = new String(Files.readAllBytes(p))
  }

  implicit class Pappend(p: Path) {
    def append(m: String): Path = {
      if(Files.exists(p)){
        Files.write(p, m.getBytes, StandardOpenOption.APPEND)
      }else{
        Files.write(p, m.getBytes)
      }
    }
  }
}


object DateImplicits {

  implicit class BigDate(time: Int) {
    def jan(): LocalDate = {
      val Year = LocalDate.now().getYear
      LocalDate.of(Year, 1, time)
    }

    def feb(): LocalDate = {
      val Year = LocalDate.now().getYear
      LocalDate.of(Year, 2, time)
    }

    def mar(): LocalDate = {
      val Year=LocalDate.now().getYear
      LocalDate.of(Year, 3, time)
    }

    def apr(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 4, time)
    }

    def may(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 5, time)
    }

    def jun(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 6, time)
    }

    def jul(): LocalDate = {
      val Year = LocalDate.now().getYear
      LocalDate.of(Year, 7, time)
    }

    def aug(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 8, time)
    }

    def sep(): LocalDate = {
      val Year= LocalDate.now().getYear()
      LocalDate.of(Year, 9, time)
    }

    def oct(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 10, time)
    }

    def nov(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 11, time)
    }

    def dec(): LocalDate = {
      val Year=LocalDate.now().getYear()
      LocalDate.of(Year, 12, time)
    }

    def jan(year: Int): LocalDate = LocalDate.of(year, 1, time)

    def feb(year: Int): LocalDate = LocalDate.of(year, 2, time)

    def mar(year: Int): LocalDate = LocalDate.of(year, 3, time)

    def apr(year: Int): LocalDate = LocalDate.of(year, 4, time)

    def may(year: Int): LocalDate = LocalDate.of(year, 5, time)

    def jun(year: Int): LocalDate = LocalDate.of(year, 6, time)

    def jul(year: Int): LocalDate = LocalDate.of(year, 7, time)

    def aug(year: Int): LocalDate = LocalDate.of(year, 8, time)

    def sep(year: Int): LocalDate = LocalDate.of(year, 9, time)

    def oct(year: Int): LocalDate = LocalDate.of(year, 10, time)

    def nov(year: Int): LocalDate = LocalDate.of(year, 11, time)

    def dec(year: Int): LocalDate = LocalDate.of(year, 12, time)
  }

  implicit class DateChange(change: Int) {
    def days(): (Int, String) = (change, "days")

    def months(): (Int, String) = (change, "months")

    def years(): (Int, String) = (change, "years")
  }

  implicit class DatePlus(date: LocalDate) {
    def +(m: (Int, String)): LocalDate = m._2 match {
      case "days"=> date.plusDays((m._1))

      case "months"=>date.plusMonths((m._1))

      case "years" => date.plusYears((m._1))
    }
  }
}