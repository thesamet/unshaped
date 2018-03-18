package scalapb.core

import com.google.protobuf.CodedOutputStream
import shapeless._

import scalapb.core3.Msg
import scalapb.macros._

/*
final class Cache {
  private final var __cachedSerializedSize: Int = 0
  final def getOrElseUpdate[T <: Msg[T]](s:  scalapb.core2.Serializer[T], t: T) = {
    var r = __cachedSerializedSize
    if (r == 0) {
      r = s.serializedSize(t)
      __cachedSerializedSize = r
    }
    r
  }
}
*/

abstract class FieldExtractors[T] {
  type G
  def extractors: G
}

object FieldExtractors {
  type Aux[T, G0] = FieldExtractors[T] {
    type G = G0
  }
}

abstract class FakeGeneric[T, G] {
  def to(t: T): G
}

trait FieldSerializer[T, SF <: SerializerFunc[T]] {
  type PT <: ProtoType

  def isDefault(t: T): Boolean

  def serializeOne(cos: CodedOutputStream, t: T): Unit

  def serialize(tag: Int): (CodedOutputStream, T) => Unit

  def mkFunc(tag: Int): SF

  def serializedSize(t: T): Int
}

abstract class SerializerFunc[T] {
  def serialize(codedOutputStream: CodedOutputStream, s: T): Unit
}

final class StringSerializer(tag: Int) extends SerializerFunc[String] {
  override final def serialize(codedOutputStream: CodedOutputStream, s: String): Unit = codedOutputStream.writeString(tag, s)
}

object FieldSerializer {
  type Aux[T, SF <: SerializerFunc[T], PT0] = FieldSerializer[T, SF] {
    type PT = PT0
  }

  /*
  def apply[PT0 <: ProtoType, T](ser: (CodedOutputStream, T) => Unit, serTag: (CodedOutputStream, Int, T) => Unit, size: T => Int, default: T => Boolean): FieldSerializer.Aux[T, PT0] =
    new FieldSerializer[T] {
      type PT = PT0

      final override def serializeOne(cos: CodedOutputStream, t: T): Unit = ser(cos, t)

      final override def serialize(tag: Int)(cos: CodedOutputStream, t: T): Unit = serTag(cos, tag, t)

      final override def isDefault(t: T): Boolean = default(t)

      final override def serializedSize(t: T): Int = size(t)
    }


  implicit val int32Serializer = FieldSerializer[ProtoType.Int32.type, Int](_.writeInt32NoTag(_), _.writeInt32(_, _), CodedOutputStream.computeInt32SizeNoTag, _ == 0)
  */

//  implicit val stringSerializer = FieldSerializer[ProtoType.String.type, String](_.writeStringNoTag(_), _.writeString(_, _), CodedOutputStream.computeStringSizeNoTag, _.isEmpty)

  implicit val stringSerializer: FieldSerializer.Aux[String, StringSerializer, ProtoType.String.type] = new FieldSerializer[String, StringSerializer] {
    override type PT = ProtoType.String.type

    final override def isDefault(t: String): Boolean = t.isEmpty

    final override def serializeOne(cos: CodedOutputStream, t: String): Unit = cos.writeStringNoTag(t)

    final override def serialize(tag: Int) = { (cos: CodedOutputStream, t: String) => cos.writeString(tag, t) }

    final override def serializedSize(t: String): Int = CodedOutputStream.computeStringSizeNoTag(t)

    final override def mkFunc(tag: Int) = new StringSerializer(tag)
  }

  /*

  implicit def optionFieldSerializer[PT <: ProtoType, T](implicit ser: FieldSerializer[PT, T]): FieldSerializer[PT, Option[T]] =
    new FieldSerializer[PT, Option[T]] {
      override def isDefault(t: Option[T]): Boolean = t.forall(ser.isDefault)

      override def serialize(cos: CodedOutputStream, tag: Int, t: Option[T]): Unit = t.foreach(ser.serialize(cos, tag, _))

      override def serializeOne(cos: CodedOutputStream, t: Option[T]): Unit = t.foreach(ser.serializeOne(cos, _))

      override def serializedSize(t: Option[T]): Int = t.map(ser.serializedSize).getOrElse(0)
    }

  implicit def messageFieldSerializer[M](implicit ser: Serializer[M]) = FieldSerializer[ProtoType.Message.type, M](
    (cos, t) => {
      cos.writeUInt32NoTag(ser.serializedSize(t))
      ser.serialize(cos, t)
    }, {
      t =>
        val size = ser.serializedSize(t)
        CodedOutputStream.computeUInt32SizeNoTag(size) + size
    }, _ => false
  )
  */
}

abstract class RepeatedFieldSerializer[PT <: ProtoType, T] {
  def isDefault(t: T): Boolean

  def serializedSize(tag: Int, t: T): Int

  def serializedPackedSize(tag: Int, t: T): Int

  def serialize(codedOutputStream: CodedOutputStream, tag: Int, t: T): Unit

  def serializePacked(codedOutputStream: CodedOutputStream, tag: Int, t: T): Unit

}

object RepeatedFieldSerializer {
  /*
  implicit def vectorFieldSerializer[PT <: ProtoType, T](implicit ser: FieldSerializer.Aux[T, PT]): RepeatedFieldSerializer[PT, Vector[T]] =
    new RepeatedFieldSerializer[PT, Vector[T]] {
      override def serialize(cos: CodedOutputStream, tag: Int, t: Vector[T]): Unit = {
        t.foreach(ser.serialize(tag)(cos, _))
      }

      override def serializedSize(tag: Int, t: Vector[T]): Int = {
        val tagSize = CodedOutputStream.computeTagSize(tag)
        t.map(ser.serializedSize).sum + t.size * tagSize
      }

      override def serializedPackedSize(tag: Int, t: Vector[T]): Int = {
        val tagSize = CodedOutputStream.computeTagSize(tag)
        val totalSize = t.map(ser.serializedSize).sum
        tagSize + totalSize + CodedOutputStream.computeUInt32SizeNoTag(totalSize)
      }

      override def serializePacked(codedOutputStream: CodedOutputStream, tag: Int, t: Vector[T]): Unit = {
        codedOutputStream.writeTag(tag, 2)
        codedOutputStream.writeUInt32NoTag(t.map(ser.serializedSize).sum)
        t.foreach(ser.serializeOne(codedOutputStream, _))
      }

      override def isDefault(t: Vector[T]): Boolean = t.isEmpty
    }
    */
}



abstract class Serializer[T] {
  type SCHEMA
  def serialize(cos: CodedOutputStream, m: T): Unit

  def toByteArray(t: T) = {
    val a = new Array[Byte](serializedSize(t))
    val outputStream = CodedOutputStream.newInstance(a)
    serialize(outputStream, t)
    outputStream.checkNoSpaceLeft()
    a
  }

  def serializedSize(t: T): Int
}

abstract class MessageSerializer[T] extends Serializer[T] {
  def serialize(cos: CodedOutputStream, m: T): Unit

  def serializedSizeNoCache(t: T): Int
}

object Serializer {
  type Aux[T, SCHEMA0] = Serializer[T] {type SCHEMA = SCHEMA0}

  implicit val SerializerHNil: Serializer.Aux[HNil, HNil] = new Serializer[HNil] {
    override type SCHEMA = HNil

    override def serialize(cos: CodedOutputStream, m: HNil): Unit = {}

    override def serializedSize(t: HNil): Int = 0
  }

  implicit def HListSerializerOptional[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList, SF <: SerializerFunc[TH]](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: FieldSerializer.Aux[TH, SF, PT],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, OptionalField[PT, TAG] :: SCHEMATAIL] = {
    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    new Serializer[TH :: TT] {
      type SCHEMA = OptionalField[PT, TAG] :: SCHEMATAIL


      override def serialize(cos: CodedOutputStream, t: TH :: TT): Unit = {
        val h = t.head
        if (!fe.isDefault(h)) {
          cos.writeTag(tag, wiretype)
          fe.serializeOne(cos, h)
        }
        tail.serialize(cos, t.tail)
      }

      override def serializedSize(t: TH :: TT): Int = {
        CodedOutputStream.computeTagSize(tag) + fe.serializedSize(t.head) + tail.serializedSize(t.tail)
      }
    }
  }

  /*
  implicit def HListSerializerRepeated[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: RepeatedFieldSerializer[PT, TH],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, RepeatedField[PT, TAG, CFalse] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      fe.serialize(cos, tag, t.head)
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedSize(tag, t.head) + tail.serializedSize(t.tail) })
  }

  implicit def HListSerializerPackedRepeated[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: RepeatedFieldSerializer[PT, TH],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, RepeatedField[PT, TAG, CTrue] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      cos.writeTag(tag, wiretype)
      fe.serializePacked(cos, tag, t.head)
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedPackedSize(tag, t.head) + tail.serializedSize(t.tail) })
  }
  */
}

trait PartialBuilder[T, SCHEMA, Fetchers <: HList] {
  def withFetchers(f: Fetchers): List[(CodedOutputStream, T) => Unit]
}

object PartialBuilder {
  implicit def nilPartialBuilder[T, S]: PartialBuilder[T, S, HNil] = new PartialBuilder[T, S, HNil] {

    /*
    def withFetchers(f: HNil): MessageSerializer[T] =
      new MessageSerializer[T] {
      override def serializedSizeNoCache(t: T): Int = 0

      override def serialize(cos: CodedOutputStream, m: T): Unit = {}

      override def serializedSize(t: T): Int = 0

      override type SCHEMA = S
    }
    */
    override def withFetchers(
      f: HNil): List[(CodedOutputStream, T) => Unit] = Nil
  }

  def funcofFunc(cos: CodedOutputStream, tag: Int, s: String): Unit = cos.writeString(tag, s)

  def mkCall[T](codedOutputStream: CodedOutputStream, s: T): Unit = codedOutputStream.writeString(21, s.asInstanceOf[String])

  implicit def mkPartialBuilder[T <: Msg[T], PT <: ProtoType, TAG <: Int, STAIL <: HList, FHEAD, FTAIL <: HList, SF <: SerializerFunc[FHEAD]](
    implicit
//    fe: FieldSerializer.Aux[FHEAD, SF, PT],
    tagWitness: Witness.Aux[TAG],
    tail: PartialBuilder[T, STAIL, FTAIL]
  ):
  PartialBuilder[T, OptionalField[PT, TAG] :: STAIL, (T => FHEAD) :: FTAIL] =
    new PartialBuilder[T, OptionalField[PT, TAG] :: STAIL, (T => FHEAD) :: FTAIL] {
      type FETCHERS = (T => FHEAD) :: FTAIL

      private type S0 = OptionalField[PT, TAG] :: STAIL
      val tag = tagWitness.value

      def withFetchers(f: FETCHERS): List[(CodedOutputStream, T) => Unit] = {
        val rec = tail.withFetchers(f.tail)
        val extract: T => FHEAD = f.head
        val fe = FieldSerializer.stringSerializer
        val mkf1 = fe.mkFunc(tag)
//        val mkf2: StringSerializer = new StringSerializer(tag)
        //        val func: (CodedOutputStream, FHEAD) => Unit = fe.serialize(tag)
        //        ((cos: CodedOutputStream, m: T) => func(cos, extract(m))) :: rec
        //                ((cos: CodedOutputStream, m: T) => funcofFunc(cos, tag, extract(m).asInstanceOf[String])) :: rec
        //        (fe.mkFunc(tag).serialize _).asInstanceOf[(CodedOutputStream, T) => Unit] :: rec
//        val e1 = ((cos: CodedOutputStream, m: T) => mkf1.serialize(cos, extract(m).asInstanceOf[String]))
//        val e2 = ((cos: CodedOutputStream, m: T) => mkf2.serialize(cos, extract(m).asInstanceOf[String]))
        ((cos: CodedOutputStream, m: T) => mkf1.serialize(cos, extract(m).asInstanceOf[String])) :: rec
//        ((cos: CodedOutputStream, m: T) => cos.writeString(tag, extract(m).asInstanceOf[String])) :: rec
//        ((cos: CodedOutputStream, m: T) => mkCall(cos, extract(m))) :: rec
      }
    }
}

      /*

          new MessageSerializer[T] {
          override type SCHEMA = S0

          override def serialize(cos: CodedOutputStream, m: T): Unit = {
            fe.serialize(cos, tag, extract(m))
            sub.serialize(cos, m)
          }

          override def serializedSizeNoCache(t: T): Int = {
            CodedOutputStream.computeTagSize(tag) + fe.serializedSize(extract(t)) + sub.serializedSize(t)
          }

          override def serializedSize(t: T): Int = {
            t.serializedSize(serializedSizeNoCache(_))
          }
        }
      }
    }
    */

trait HeadThing[T]

object HeadThing {
  implicit def mk[T]: HeadThing[T] = new HeadThing[T] {}
}

/*
trait PartialBuilder[T] {
  type FETCHERS <: HList
//  type SCHEMA
}

object PartialBuilder {
  type Aux[F0, T] = PartialBuilder[T] {
    type FETCHERS = F0
//    type SCHEMA = S0
  }

  implicit def nilPartialBuilder[T <: Msg[T]]: PartialBuilder.Aux[HNil, T] = new PartialBuilder[T] {
    type FETCHERS = HNil
//    override type SCHEMA = HNil
  }

  implicit def mkPartialBuilder[T, FHEAD, FTAIL <: HList](implicit headThing: HeadThing[FHEAD], tail: PartialBuilder.Aux[FTAIL, T]): PartialBuilder.Aux[FHEAD :: FTAIL, T] =
    new PartialBuilder[T] {
      type FETCHERS = FHEAD :: FTAIL
//      override type SCHEMA = SHEAD :: STAIL
    }
}
*/

object MessageSerializer {
  implicit def fromHelper[T <: Msg[T], SCHEMA0 <: HList, G <: HList](
    implicit helper: SchemaHolder.Aux[T, SCHEMA0], gen: FieldExtractors.Aux[T, G], pr: PartialBuilder[T, SCHEMA0, G]): MessageSerializer[T] = {
    val f = pr.withFetchers(gen.extractors).toArray
    /*
    val fe1 :: fe2 :: fe3 :: HNil = gen.fetchers
    val f = Array(
      (cos: CodedOutputStream, t: T) => cos.writeString(37, fe1.asInstanceOf[T => String](t)),
      (cos: CodedOutputStream, t: T) => cos.writeString(45, fe2.asInstanceOf[T => String](t)),
      (cos: CodedOutputStream, t: T) => cos.writeString(73, fe3.asInstanceOf[T => String](t))
    )
    */

    new MessageSerializer[T] {
      override def serialize(cos: CodedOutputStream, m: T): Unit = {
        var i = 0
        while (i < f.length) {
          f(i)(cos, m)
          i += 1

        }
        /*
        var x = f
        while (!x.isEmpty) {
          (x.head)(cos, m)
          x = x.tail
        }
        */
//        f.foreach(_(cos, m))
      }

      override def serializedSizeNoCache(t: T): Int = 21

      override type SCHEMA = SCHEMA0

      override def serializedSize(t: T): Int = 21
    }
  }
  /*
    new MessageSerializer[T] {
      type SCHEMA = SCHEMA0

      override def serialize(cos: CodedOutputStream, m: T): Unit = {
//        pr.serialize(cos, gen.to(m))
      }

      override def serializedSize(t: T): Int = 0 // t.serializedSize(serializedSizeNoCache)

      override def serializedSizeNoCache(t: T): Int = 0 // pr.serializedSize(gen.to(t))
    }
    */
}

