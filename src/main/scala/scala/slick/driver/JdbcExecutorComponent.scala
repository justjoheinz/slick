package scala.slick.driver

import scala.collection.mutable.Builder
import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.util.SQLBuilder
import scala.slick.profile.SqlExecutorComponent
import scala.slick.lifted.{Shape, ShapeLevel}
import scala.slick.ast.First
import scala.slick.ast.ResultSetMapping
import scala.slick.ast.CompiledStatement

trait JdbcExecutorComponent extends SqlExecutorComponent { driver: JdbcDriver =>

  type QueryExecutor[T] = QueryExecutorDef[T]
  type UnshapedQueryExecutor[T] = UnshapedQueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createUnshapedQueryExecutor[M](value: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutorDef[M](value)

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {

    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql

    def run(implicit session: Backend#Session): R = tree match {
      case rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType)) :@ CollectionType(cons, el) =>
        val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, R]]
        createQueryInvoker[Any, Any](rsm).foreach(param, { x => b += x }, 0)(session)
        b.result()
      case First(rsm: ResultSetMapping) =>
        createQueryInvoker[Any, R](rsm).first(param)
    }
  }

  class UnshapedQueryExecutorDef[M](value: M) extends super.UnshapedQueryExecutorDef[M](value) {
    @inline final def selectStatement[U](implicit shape: Shape[_ <: ShapeLevel.Flat, M, U, _], session: Backend#Session): String =
      executor[U].selectStatement
  }
}
