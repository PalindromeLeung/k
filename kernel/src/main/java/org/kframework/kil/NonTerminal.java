// Copyright (c) K Team. All Rights Reserved.
package org.kframework.kil;

import java.util.Optional;
import org.kframework.kore.Sort;
import scala.Option;

/** A nonterminal in a {@link Production}. Also abused in some places as a sort identifier */
public class NonTerminal extends ProductionItem {

  private Sort sort;
  private final Optional<String> name;

  public NonTerminal(Sort sort, Optional<String> name) {
    super();
    this.sort = sort;
    this.name = name;
  }

  public Option<String> getName() {
    if (name.isPresent()) {
      return Option.apply(name.get());
    }
    return Option.empty();
  }

  public void setSort(Sort sort) {
    this.sort = sort;
  }

  public Sort getSort() {
    return sort;
  }

  public Sort getRealSort() {
    return sort;
  }

  @Override
  public void toString(StringBuilder sb) {
    if (name.isPresent()) {
      sb.append(name.get()).append(": ");
    }
    sb.append(sort);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) return false;
    if (obj == this) return true;
    if (!(obj instanceof NonTerminal nt)) return false;

    return sort.equals(nt.sort);
  }

  @Override
  public int hashCode() {
    return sort.hashCode();
  }
}
