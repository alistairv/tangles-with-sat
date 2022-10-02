package code

import ujson.*
import os.*

/**
 * This class reads a swap list in JSON representation and constructs an equivalent
 * 2D sequence. For the reading, the uJson module of the uPickle library is used.
 * Each Reader instance is bound to a single path passed to it during construction
 * time. Data can be read through the read() function. Negative entries in the swap
 * list are allowed, floating point numbers are not.
 * @param path the path to the JSON file
 */
class Reader(path: Path):
  def read(): Seq[Seq[Int]] =
    ujson.read(os.read(path)) // Value=Arr of Value=Arr of Value=Num
      .arr                           // Arr of ...
      .value                         // ArrayBuffer[...]
      .map(
        _.arr                        // Arr of Value=Num
          .value                     // ArrayBuffer[Value=Num]
          .toSeq                     // Seq[Value=Num]
          .map(_.num.toInt)          // Seq[Int]
      ).toSeq                        // Seq[Seq[Int]]

object Reader:
  def apply(path: Path): Reader = new Reader(path)
