package input_file

import scala.io.Source

object InputFileParser {


  def apply(filename: String) ={
    var list, input_file: List[String] = List()
    var input = ""

    val bufferedSource = Source.fromFile(filename)
    for(line <- bufferedSource.getLines){
      if(line.size > 0)  input_file = input_file :+ line.trim
    }
    bufferedSource.close()

    var map:Map[Int, String] = Map()
    for(i<-0 until input_file.size){
      map = map + (i+1 -> input_file(i))
    }

    var while_c,if_c,start_if, start_while, stop_if, stop_while = 0
    var stmt_index_list:List[(Int, Int)] = List()

    val seq:Seq[(Int, String)] = map.toSeq.sortBy(_._1)

    for(i<-0 until seq.size){
      val e = seq(i)
      if(e._2.trim.take(2).equals("if")) {
        if(while_c == 0) {
          if(if_c == 0) {
            if_c = if_c + 1
            start_if = e._1
          }
          else if_c = if_c +1
        }
      }

      if(e._2.trim.take(6).equals("endif;")){
        if(while_c == 0){
          if(if_c == 1) {
            stop_if = e._1
            if_c = if_c - 1
            stmt_index_list = stmt_index_list :+ (start_if,stop_if)
          }
          else if_c = if_c - 1
        }
      }

      if(e._2.trim.take(5).equals("while")) {
        if(if_c == 0){
          if(while_c == 0){
            while_c = while_c + 1
            start_while = e._1
          }
          else while_c = while_c + 1
        }
      }

      if(e._2.trim.take(9).equals("endwhile;")) {
        if(if_c == 0) {
          if(while_c == 1) {
            stop_while = e._1
            while_c = while_c - 1
            stmt_index_list = stmt_index_list :+ (start_while, stop_while)
          }
          else while_c = while_c -1
        }
      }

      if( i == seq.size -1) {
        if(if_c < 0) throw new Exception("You have "+ -if_c +"\"if" + "\"if\" statement unfinished (\"endif;\" missing)!!")
        if(while_c < 0) throw new Exception("You have "+ -while_c +"\"if" + "\"if\" statement unfinished (\"endwhile;\" missing)!!")
        if(if_c > 0) throw new Exception("You have "+ if_c + "\"if\" statement start and unfinished!!")
        if(while_c > 0) throw new Exception("You have "+ while_c + "\"if\" statement start and unfinished!!")
      }

    }

    def complete_start_between_stmt_end(start: Int,stop: Int, map: Map[Int, String]) = {
      var lst: List[String] = List()
      for(i<-start until stop){
        lst = lst :+ map.get(i).get
      }
      lst
    }

    def complete_stmt(start: Int, stop: Int, map: Map[Int, String]) = {
      var lst = ""
      for(i<-start to stop){
        lst = lst.concat(map.get(i).get).concat(" ")
      }
      lst
    }

    var output: List[String] = List()
    if(stmt_index_list.size > 0){
      for(i<-0 until stmt_index_list.size){
        val start_curr = stmt_index_list(i)._1
        val stop_curr = stmt_index_list(i)._2

        if(i == 0){
          output = output ++ complete_start_between_stmt_end(1, start_curr, map)
          if(start_curr < stop_curr) output = output ++ List(complete_stmt(start_curr,stop_curr,map))
        }

        if(i > 0){
          val start_prev = stmt_index_list(i-1)._1
          val stop_prev = stmt_index_list(i-1)._2
          if(stop_prev < start_curr) output = output ++ complete_start_between_stmt_end(stop_prev +1, start_curr, map)
          if(start_curr < stop_curr) output = output ++ List(complete_stmt(start_curr, stop_curr, map))
        }

        if(i == stmt_index_list.size - 1) output = output ++ complete_start_between_stmt_end(stop_curr +1, map.size +1, map)
      }
    }
    else
      {
       for(i<-1 to map.size){
         output = output ++ List(map.get(i).get)
       }
      }
    output
  }

}
