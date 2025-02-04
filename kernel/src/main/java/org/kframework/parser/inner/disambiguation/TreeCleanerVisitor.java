// Copyright (c) K Team. All Rights Reserved.
package org.kframework.parser.inner.disambiguation;

import org.kframework.parser.Ambiguity;
import org.kframework.parser.SafeTransformer;
import org.kframework.parser.Term;

/** Remove parsing artifacts such as single element ambiguities. */
public class TreeCleanerVisitor extends SafeTransformer {
  @Override
  public Term apply(Ambiguity amb) {
    Term newTerm = super.apply(amb);
    if (newTerm instanceof Ambiguity newAmb) {
      if (newAmb.items().size() == 1) {
        return newAmb.items().iterator().next();
      }
    }
    return newTerm;
  }
}
