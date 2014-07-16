// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import org.kframework.backend.java.builtins.BoolToken;
import org.kframework.backend.java.builtins.IntToken;
import org.kframework.backend.java.builtins.UninterpretedToken;
import org.kframework.backend.java.symbolic.Matcher;
import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Unifier;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.backend.java.util.Utils;
import org.kframework.kil.ASTNode;

import java.util.Collections;
import java.util.Set;


/**
 * A K term of the form {@code SORT(#"VALUE")}.
 *
 * @author AndreiS
 */
public abstract class Token extends Term implements Immutable {

    public static Token of(String sort, String value) {
        switch (sort) {
            case BoolToken.SORT_NAME:
                return BoolToken.of(Boolean.parseBoolean(value));
            case IntToken.SORT_NAME:
                return IntToken.of(value);
            default:
                return UninterpretedToken.of(sort, value);
        }
    }

    public Token() {
        super(Kind.KITEM);
    }

    @Override
    public boolean isExactSort() {
        return true;
    }

    /**
     * Returns a {@code String} representation of the sort of this token.
     */
    @Override
    public abstract String sort();

    /**
     * Returns a {@code String} representation of the (uninterpreted) value of this token.
     */
    public abstract String value();

    @Override
    public final boolean isGround() {
        return true;
    }

    @Override
    public final boolean isSymbolic() {
        return false;
    }

    @Override
    public Set<Variable> variableSet() {
        return Collections.emptySet();
    }

    @Override
    protected final boolean computeHasCell() {
        return false;
    }

    @Override
    public void accept(Unifier unifier, Term pattern) {
        unifier.unify(this, pattern);
    }

    @Override
    public void accept(Matcher matcher, Term pattern) {
        matcher.match(this, pattern);
    }

    @Override
    public ASTNode accept(Transformer transformer) {
        return transformer.transform(this);
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }

    @Override
    public String toString() {
        return sort() + "(#\"" + value() + "\")";
    }

}
