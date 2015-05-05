

import oatLib.db.DB
import anorm._
import org.postgresql.util.PGobject
import anorm.MayErr._
import java.sql.Connection


class DbEnum extends Enumeration {

    // Convert a SQL enum into a scala enum
    protected def enumToType[E](convert: String => E)(implicit m: Manifest[E]): Column[E] = Column {
        (value, meta) =>
            val MetaDataItem(qualified, nullable, clazz) = meta

            try { 
                val s = value.asInstanceOf[String]
                eitherToError(Right(convert(s))): MayErr[SqlRequestError, E]
            } catch {
              case e: Exception => 
                eitherToError(Left(TypeDoesNotMatch("Cannot convert " + value + ":" + 
                    value.asInstanceOf[AnyRef].getClass + " to " + 
                    m.runtimeClass.getSimpleName + "for column " + 
                    qualified)))
            }
    }

    // Create a method to interpolate a Scala enum into SQL.
    protected def createEnumToStatement[E]() = new ToStatement[E] {
        def set(s: java.sql.PreparedStatement, index: Int, aValue: E): Unit = {
            s.setObject(index, aValue.toString, java.sql.Types.OTHER)   
        }   
    }

}

object NoteCategory extends DbEnum {
    type NoteCategory = Value

    val Auto, Advisor, CourseEntry = Value

    implicit val noteCategoryToStatement = createEnumToStatement[NoteCategory]()
    implicit val rowToNoteCategory = enumToType[NoteCategory](NoteCategory.withName)
}

object SortOrder extends DbEnum {
    type SortOrder = Value

    val Ascending = Value("ASC")
    val Descending = Value("DESC")

    implicit val sortOrderToStatement = createEnumToStatement[SortOrder]()
    implicit val rowToSortOrder = enumToType[SortOrder](SortOrder.withName)
}


object Main {
    import NoteCategory._
    import SortOrder._

    def main(args:Array[String]):Unit = {

        val db = DB("local_dev")

        db.withConnection { implicit conn =>

            // Delete everything from the table
            SQL"""truncate _oat.test_enum""".execute()

            // Insert one of each
            val sql_1 = SQL"""insert into _oat.test_enum (note_category, sort_order) 
                    VALUES  (${NoteCategory.Advisor}, ${SortOrder.Ascending}) RETURNING id"""
            val id_1 = sql_1.as(anorm.SqlParser.scalar[Long].singleOpt) 

            // Read them back and verify
            val r_1 = SQL"""select * from _oat.test_enum where id = ${id_1}""".apply().head
            val (nc_1, so_1) = (r_1[NoteCategory]("note_category"), r_1[Option[SortOrder]]("sort_order"))
            assert(nc_1 == NoteCategory.Advisor)
            assert(so_1 == Some(SortOrder.Ascending))



            // Insert a note category and a null
            val sql_2 = SQL"""insert into _oat.test_enum (note_category, sort_order) 
                    VALUES  (${NoteCategory.Auto}, NULL) RETURNING id"""
            val id_2 = sql_2.as(anorm.SqlParser.scalar[Long].singleOpt) 

            // Read them back and verify
            val r_2 = SQL"""select * from _oat.test_enum where id = ${id_2}""".apply().head
            val (nc_2, so_2) = (r_2[NoteCategory]("note_category"), r_2[Option[SortOrder]]("sort_order"))
            assert(nc_2 == NoteCategory.Auto)
            assert(so_2 == None)



            // Update with new values
            val sql_3 = SQL"""update _oat.test_enum 
                    set (note_category, sort_order) = (${NoteCategory.CourseEntry}, ${SortOrder.Descending})
                    where id = $id_2""".execute

            // Read them back and verify
            val r_3 = SQL"""select * from _oat.test_enum where id = ${id_2}""".apply().head
            val (nc_3, so_3) = (r_3[NoteCategory]("note_category"), r_3[Option[SortOrder]]("sort_order"))
            assert(nc_3 == NoteCategory.CourseEntry)
            assert(so_3 == Some(SortOrder.Descending))
        }
    }
}