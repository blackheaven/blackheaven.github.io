module Opaque where

import Polysemy
import Polysemy.Opaque
import Polysemy.Trace

-- wrong :: Sem (e ': Trace ': r) ()
-- wrong = trace "Wrong"

ok :: Sem (e ': Trace ': r) ()
ok = fromOpaque $ trace "Works"
