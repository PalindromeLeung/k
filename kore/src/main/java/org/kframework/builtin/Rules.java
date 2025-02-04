// Copyright (c) K Team. All Rights Reserved.
package org.kframework.builtin;

import static org.kframework.kore.KORE.*;

import org.kframework.kore.KRewrite;

public class Rules {
  public static final KRewrite STUCK_RULE =
      KRewrite(
          KApply(
              KLabels.STRATEGY_CELL,
              KList(
                  KApply(KLabels.KSEQ, KList(KApply(KLabels.DOTK, KList()), KVariable("#REST"))))),
          KApply(
              KLabels.STRATEGY_CELL,
              KList(
                  KApply(
                      KLabels.KSEQ, KList(KApply(KLabels.STUCK, KList()), KVariable("#REST"))))));
}
