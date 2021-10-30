namespace Smartian

type ConcolicQueue = Queue<Seed>

module ConcolicQueue =

  let empty: ConcolicQueue =
    Queue.empty

  let size (queue: ConcolicQueue) =
    Queue.size queue

  let isEmpty (queue: ConcolicQueue) =
    Queue.isEmpty queue

  let enqueue (queue: ConcolicQueue) seed =
    if not (Seed.isInputCursorValid seed) then queue
    else Queue.enqueue queue seed

  let dequeue (queue: ConcolicQueue) =
    Queue.dequeue queue

type RandFuzzQueue = DurableQueue<Seed>

module RandFuzzQueue =

  let init (): RandFuzzQueue =
    DurableQueue.initialize Seed.empty

  let size (queue: RandFuzzQueue) =
    DurableQueue.size queue

  let enqueue (queue: RandFuzzQueue) seed =
    DurableQueue.enqueue queue seed

  let fetch (queue: RandFuzzQueue) =
    DurableQueue.fetch queue
