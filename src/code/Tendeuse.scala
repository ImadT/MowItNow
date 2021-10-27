// Scala File handling package
import scala.io.Source
import java.io.FileNotFoundException
// Scala collection package
import scala.collection.mutable


/*
 * Déclaration de la classe Tendeuse avec pour axes : x et y
 * x: La position de la tendeuse sur l'axe x
 * y: La position de la tendeuse sur l'axe y
 * Orientation de la tondeuse: il s'agit de la notation cardinale anglaise.  elle admet comme valeur les caractères :(N,E,W,S)
 * Pelouse: Elle correspond aux coordonnées du coin supérieur à droite avec lequel on peut définir les dimensions de la pelouse
 * list_instructions: c'est la liste des instructions données par le codeur de la tendeuse qui sont D, G, A. Il s'agit d'un tableau de caractères.
 */
class Tendeuse(var x:Int = 0, var y:Int = 0, var orientation:Char = 'N', var pelouse:Array[Int] = Array(3,3), list_instructions:Array[Char] = Array()){

  /*
  * Définition de la methode "instruction_suivante":
  * la methode applique une instruction à la tendeuse pour effectuer un mouvement.
  * mouvement_suivant : Argument de la méthode "instruction_suivante" sous forme d'un caractère. il prend une valeur parmi (A,D,G)
  * type de retour : Unit ( rien )
  */
  def instruction_suivante(mouvement_suivant: Char): Unit ={
    /*
    * case et pattern matching pour la variable "mouvement_suivant"
     */
    mouvement_suivant match{
      /*
      * Dans le cas ou mouvement_suivant = 'A'
       */
      case 'A' =>
        /*
        * case et pattern matching pour la variable "orientation"
        */
        orientation match {
          case 'N' => if (y<pelouse(1)) y = y + 1   // Dans le cas où la tendeuse se trouve dans la pelouse, on effectue un mouvement
          case 'E' => if (x<pelouse(0)) x = x + 1
          case 'W' => if (x>0) x = x - 1
          case 'S' => if (y>0) y = y - 1
        }
        /*
        * Dans le cas ou mouvement_suivant = 'D'
         */
      case 'D' =>
        orientation = orientation match {
          case 'N' => 'E'
          case 'E' => 'S'
          case 'W' => 'N'
          case 'S' => 'W'
        }
        /*
        * Dans le cas ou mouvement_suivant = 'G'
         */
      case 'G' =>
        orientation = orientation match {
          case 'N' => 'W'
          case 'E' => 'N'
          case 'W' => 'S'
          case 'S' => 'E'
        }
      case _ => println("aucun mouvement ne sera possible")
    }
  }

  /*
  * La méthode executer_les_instructions:
  * elle execute la liste des instructions.
  * le type de retour : Unit
   */
  def  executer_les_instructions(): Unit ={
    // Si la tendeuse se trouve initialement dans la pelouse alors on effectue le process de mouvement
    if(y>=0 && y<=pelouse(1) && x>=0 && x<=pelouse(0)) {
      for (i <- list_instructions) {
        instruction_suivante(i)
      }
    }
    // si la tendeuse ne se trouve pas initialamnet dans la pelouse l'utilisateur reçoit un message :
    else {
      println("La position initiale de la tendeuse tombe dehors la pelouse, du coup aucun déplacement ne sera effectué")
    }
  }
}


object Execution extends App {
  // la collection pour stocker les objets "Tendeuse".
  val tendeusesMap:mutable.Map[String,Tendeuse] = scala.collection.mutable.Map()


  // instanciation des variables.
  var iterator_ligne:Int = 1 // sert comme compteur des lignes recuperées à partir du Buffer "fSource"
  var x = 0
  var y = 0
  var orientation = 'N'
  var pelouse = new Array[Int](2)
  var instructions:Array[Char] = Array()


  // Block try/Catch
  try{
    // fSource est une source tamponnée
    val fSource = Source.fromFile("./src/resources/instructions.txt")
    println("\nLe contenu du fichier de pilotage est :")

    // récupérer les informations du fichier de pilotage donné en entrée.
    for(line<-fSource.getLines)
    {
      if (iterator_ligne == 1){ // récupérer la dimension du coin supérieur de la pelouse
        val pelouseStr:Array[String] = line.split(" ") // récupérer la première ligne + diviser par l'espace
        //2 méthodes sont envisageables :
        // méthode 1/ pelouse = for ( element <- pelouseStr ) yield element.toInt
        // méthode 2/ map pour convertir les éléments du tableau "pelouseStr" de "String" vers "Int" : celle que nous utiliserons ici
        pelouse = pelouseStr.map(_.toInt)


        iterator_ligne +=1
      }
      else{
        if(iterator_ligne%2==0){  // Si la ligne est paire alors il s'agit de l'information sur la position initiale de la tendeuse
          val position_initial_du_tendeuse:Array[String] = line.split(" ")
          x = position_initial_du_tendeuse(0).toInt
          y = position_initial_du_tendeuse(1).toInt
          if(position_initial_du_tendeuse(2).charAt(0).isLetter){
            orientation = position_initial_du_tendeuse(2).charAt(0)
          }
          else{
            // lancer une exception
            throw new Exception(s"Erreur s'est produite : à cause de la ligne $iterator_ligne du fichier de pilotage, faut saisir une lettre pour l'orientation !")
          }


          iterator_ligne +=1
        }
        else{
          instructions = line.toCharArray  //Array(line).map(_.charAt(0))
          // ajouter le suivant tendeuse dans tendeusesMap
          tendeusesMap(f"Tendeuse ${iterator_ligne/2}") = new Tendeuse(x,y,orientation,pelouse,instructions)
          iterator_ligne +=1
        }
      }
      println(line)
    }
    println("\n\n")



    println("La position finale des tondeuses :")
    // Execution des instructions pour chaque objet tendeuse stoqké dans tendeusesMap
    for ((nom_tendeuse,tendeuse_i) <- tendeusesMap){
      tendeuse_i.executer_les_instructions()
      println(nom_tendeuse + " : " + tendeuse_i.x + " " + tendeuse_i.y + " " + tendeuse_i.orientation)
    }

    // closing file
    fSource.close()
  } catch {
    case _ : FileNotFoundException =>  println("Une erreur s'est produite : le chemin fourni pour récupérer le pilotage est erroné")//ex.printStackTrace()
    case _ : NumberFormatException  => println(f"Une erreur s'est produite : à revoir la ligne $iterator_ligne du fichier de pilotage, faut saisir des chiffres pour indiquer la position de la tendeuse ou bien les dimensions de la pelouse!")
    case e : Throwable => println(e)
  }
}
