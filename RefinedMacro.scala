package fix

import scala.meta.Defn
import scala.meta.Lit
import scala.meta.Member
import scala.meta.Mod
import scala.meta.Term
import scala.meta.Type
import scalafix.Patch
import scalafix.lint.Diagnostic
import scalafix.lint.LintSeverity
import scalafix.v1.SyntacticDocument
import scalafix.v1.SyntacticRule

class RefinedMacro extends SyntacticRule("RefinedMacro") {
  override def fix(implicit doc: SyntacticDocument): Patch = {
    doc.tree.collect {
      case t @ Defn.Def.After_4_7_3(
            methodMods,
            methodName: Term.Name,
            List(
              Member.ParamClauseGroup(
                typeParams,
                List(
                  Term.ParamClause(
                    _,
                    Some(paramMods)
                  )
                )
              )
            ),
            Some(
              Type.Apply.After_4_6_0(
                Type.Select(Term.Name("Validate"), Type.Name("Aux")) | Term.Name("Validate"),
                _
              )
            ),
            _
          ) =>
        if (methodMods.exists(_.is[Mod.Implicit]) && paramMods.is[Mod.Implicit]) {
          Patch.lint(
            Diagnostic(
              id = "",
              message = "macroに書き換え必要?",
              position = t.pos,
              severity = LintSeverity.Error
            )
          )
        } else {
          Patch.empty
        }
    }.asPatch
  }
}
