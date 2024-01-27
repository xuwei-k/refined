import scala.meta.Lit
import scala.meta.Source
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.XtensionQuasiquoteImporter
import scala.meta.inputs.Input
import scala.meta.parsers.Parse
import scalafix.Patch
import scalafix.internal.config.ScalaVersion
import scalafix.v1.SyntacticDocument

object SingletonTypeCompat {
  private def createPatch(tree: Tree): Patch = Seq(
    /*
    Patch.addGlobalImport(
      importer"eu.timepit.refined.W"
    ),

     */
    tree.collect { case Type.ArgClause(args) =>
      args.collect { case n: Lit =>
        Patch.replaceTree(
          n,
          Type
            .Select(
              Term.Select(Term.Name("W"), Term.Name(n.toString())),
              Type.Name("T")
            )
            .toString
        )
      }.asPatch
    }.asPatch
  ).asPatch

  def fix(input: Input): String = {
    val parse = implicitly[Parse[Source]]
    val tree = parse(input = input, dialect = scala.meta.dialects.Scala3).get
    val doc = SyntacticDocument.fromTree(tree)
    val patch = createPatch(doc.tree)
    scalafix.internal.patch.PatchInternals
      .syntactic(
        Map(scalafix.rule.RuleName("SingletonTypeCompat") -> patch),
        doc,
        false
      )
      .fixed
  }
}
