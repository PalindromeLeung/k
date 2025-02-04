// Copyright (c) K Team. All Rights Reserved.
package org.kframework.compile.checks;

import static org.kframework.Collections.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.kframework.definition.Module;
import org.kframework.definition.Sentence;
import org.kframework.definition.SyntaxAssociativity;
import org.kframework.definition.Tag;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.errorsystem.KExceptionManager;

public record CheckSyntaxGroups(Set<KEMException> errors, Module module, KExceptionManager kem) {

  public void check(Sentence s) {
    if (s instanceof SyntaxAssociativity) {
      Set<Tag> tags = mutable(((SyntaxAssociativity) s).tags());
      List<Tag> tagList = new ArrayList<>(tags);

      for (int i = 0; i < tagList.size(); ++i) {
        for (int j = i + 1; j < tagList.size(); ++j) {
          Tag t1 = tagList.get(i);
          Tag t2 = tagList.get(j);

          if (this.module.priorities().inSomeRelation(t1, t2)) {
            String message =
                "Symbols "
                    + t1
                    + " and "
                    + t2
                    + " are in the same associativity group, but have different priorities.";
            kem.registerCompilerWarning(
                KException.ExceptionType.INVALID_ASSOCIATIVITY, errors, message, s);
          }
        }
      }
    }
  }
}
