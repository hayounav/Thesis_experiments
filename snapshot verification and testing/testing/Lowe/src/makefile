CADS = ../ox/cads

ATOMICDIR = $(CADS)/atomic
ATOMICS = $(ATOMICDIR)/AtomicPair.class

# Utilities

UTILDIR = $(CADS)/util
UTILS = $(UTILDIR)/ThreadUtil.class $(UTILDIR)/Backoff.class			\
$(UTILDIR)/ThreadID.class $(UTILDIR)/Profiler.class $(UTILDIR)/Log.class	\
$(UTILDIR)/CombiningTree.class $(UTILDIR)/Counter.class				\
$(UTILDIR)/NanoSpin.class $(UTILDIR)/Spin.class

# Locks

LOCKDIR = $(CADS)/locks
LOCKS = $(LOCKDIR)/Condition.class $(LOCKDIR)/Lock.class		\
$(LOCKDIR)/SimpleCondition.class $(LOCKDIR)/TASLock.class		\
$(LOCKDIR)/TTASLock.class $(LOCKDIR)/BackoffLock.class			\
$(LOCKDIR)/PetersonLock.class $(LOCKDIR)/ArrayQueueLock.class		\
$(LOCKDIR)/SimpleArrayQueueLock.class $(LOCKDIR)/SimpleDelayLock.class	\
$(LOCKDIR)/BakeryLock.class $(LOCKDIR)/ReadWriteLock.class		\
$(LOCKDIR)/SimpleReadWriteLock.class $(LOCKDIR)/FairReadWriteLock.class	\
$(LOCKDIR)/SimpleReentrantLock.class $(LOCKDIR)/Semaphore.class		\
$(LOCKDIR)/CLHLock.class

# $(LOCKDIR)/PetersonLock0.class 

# Linearizability testing
# -----------------------

TESTDIR = $(CADS)/testing

# Components of testing framework
TESTING_CPTS = $(TESTDIR)/ThreadStates.class $(TESTDIR)/StateWrapper.class	\
$(TESTDIR)/Event.class $(TESTDIR)/Configuration.class				\
$(TESTDIR)/ConfigSet.class $(TESTDIR)/MyBitMapSet.class				\
$(TESTDIR)/UndoConfig.class $(TESTDIR)/QLTHistory.class

# Logging for sequential generic testers
TESTING_GEN_LOG = $(TESTDIR)/GenericThreadLog.class		\
$(TESTDIR)/GenericLog.class $(TESTDIR)/SharedLog.class				\
$(TESTDIR)/TSThreadLog.class $(TESTDIR)/TSLog.class

#Logging for sequential queue-oriented testers
TESTING_QUEUE_LOG = $(TESTDIR)/QueueThreadLog.class $(TESTDIR)/QueueLog.class	\
$(TESTDIR)/SharedQueueLog.class $(TESTDIR)/QueueTSThreadLog.class		\
$(TESTDIR)/QueueTSLog.class

# Logging for generic competition testers
TESTING_COMP_LOG = $(TESTDIR)/CompetitionThreadLog.class			\
$(TESTDIR)/CompetitionLog.class $(TESTDIR)/SharedCompetitionLog.class		\
$(TESTDIR)/TSCompetitionThreadLog.class $(TESTDIR)/TSCompetitionLog.class

TESTING_QUEUE_COMP_LOG = $(TESTDIR)/QueueCompetitionThreadLog.class	\
$(TESTDIR)/QueueCompetitionLog.class					\
$(TESTDIR)/SharedQueueCompetitionLog.class				\
$(TESTDIR)/TSQueueCompetitionThreadLog.class				\
$(TESTDIR)/TSQueueCompetitionLog.class

# Logging (all)
TESTING_LOGS = $(TESTDIR)/TS.class $(TESTING_GEN_LOG) $(TESTING_QUEUE_LOG)	\
$(TESTING_COMP_LOG) $(TESTING_QUEUE_COMP_LOG)

# Solvers for linearizability testing
TESTING_SOLVERS = $(TESTDIR)/Solver.class 		\
$(TESTDIR)/GenericSolver.class $(TESTDIR)/WGGraph.class				\
$(TESTDIR)/BFSJITLinTester.class $(TESTDIR)/DFSGraphJITLinTester.class		\
$(TESTDIR)/WGLinearizabilityTester.class $(TESTDIR)/JITLinUndoTester.class	\
$(TESTDIR)/QueueLinSolver.class $(TESTDIR)/CompetitionSolver.class

# Redundant: $(TESTDIR)/LinTester.class $(TESTDIR)/UndoingLinTester.class
# $(TESTDIR)/DFSJITLinTester.class
# $(TESTDIR)/CompetitionLinearizabilityTester.class
# $(TESTDIR)/DFSLinearizabilityTester.class $(TESTDIR)/Log.class	\
# $(TESTDIR)/CompTesterLog.class \

TESTING = $(TESTING_CPTS) $(TESTING_LOGS) $(TESTING_SOLVERS)			\
$(TESTDIR)/LinearizabilityTester.class $(TESTDIR)/QueueLinTester.class		\
$(TESTDIR)/CompetitionTester.class $(TESTDIR)/QueueCompetitionTester.class

#  $(TESTDIR)/StackPool.class	\ $(TESTDIR)/LockFreeQueue1RSpin.class	
# $(TESTDIR)/WaitingStackPool.class $(TESTDIR)/ConcurrentStateWrapper.class	\
# $(TESTDIR)/ConcurrentDFSLinearizabilityTester.class

# Collections
# -----------

COLLDIR = $(CADS)/collection

SETS = $(COLLDIR)/Set.class $(COLLDIR)/CoarseListSet.class		\
$(COLLDIR)/FineListSet.class $(COLLDIR)/OptimisticListSet.class		\
$(COLLDIR)/LazyListSet.class $(COLLDIR)/LockFreeListSet.class		\

QUEUES = $(COLLDIR)/Queue.class $(COLLDIR)/PartialQueue.class			\
$(COLLDIR)/LockFreeQueue.class $(COLLDIR)/BoundedQueue.class			\
$(COLLDIR)/UnboundedQueue.class $(COLLDIR)/LockFreeQueueRecycle.class		\
$(COLLDIR)/SynchronousChannel.class $(COLLDIR)/SynchronousDualChannel.class

STACKS = $(COLLDIR)/Stack.class $(COLLDIR)/TotalStack.class		\
$(COLLDIR)/LockFreeStack.class $(COLLDIR)/LockFreeExchanger.class	\
$(COLLDIR)/EliminationBackoffStack.class

MAPS = $(COLLDIR)/HashMap.class $(COLLDIR)/ShardedMap.class			\
$(COLLDIR)/LockFreeReadShardedMap.class $(COLLDIR)/LvdPWMap.class		\
$(COLLDIR)/LvdPWResizeMap.class $(COLLDIR)/RecursiveSplitOrderingMap.class	\

SKIPLISTS = $(COLLDIR)/LazySkipList.class $(COLLDIR)/LockFreeSkipList.class	\
$(COLLDIR)/CollisionResistantLazySkipList.class					\
$(COLLDIR)/CollisionResistantLockFreeSkipList.class

PRIQUEUES = $(COLLDIR)/PriorityQueue.class $(COLLDIR)/TreePriorityQueue.class

HASHSETS = $(COLLDIR)/HashSet.class $(COLLDIR)/ShardedSet.class	\
$(COLLDIR)/LockFreeReadShardedSet.class

MISCCOLL = $(COLLDIR)/Pool.class $(COLLDIR)/Termination.class			\
$(COLLDIR)/TerminationDetectingPool.class 	\
$(COLLDIR)/BoundedBuffer.class

COLLS = $(MISCCOLL) $(MAPS) $(SETS) $(QUEUES) $(STACKS) $(SKIPLISTS)	\
$(PRIQUEUES) $(HASHSETS) 

# Moved to ../Removed: $(COLLDIR)/Map.class $(COLLDIR)/ShardedMap.class \
# $(COLLDIR)/ShardedMapTTASLocks.class $(COLLDIR)/DeQueue.class

# Experiments

EXPDIR = $(CADS)/experiments
EXPS = $(EXPDIR)/specialFunctions.class $(EXPDIR)/ConfidenceIntervals.class	\
$(EXPDIR)/Experiments.class $(EXPDIR)/Graphs.class

COMPONENTS = $(ATOMICS) $(COLLDIR)/GrowableArray.class $(UTILS) $(LOCKS) $(COLLS) $(TESTING) $(EXPS)


all: $(COMPONENTS)

doc:
	scaladoc -d ../doc util/*.scala atomic/*.scala locks/*.scala experiments/*.scala collection/*.scala testing/*.scala

# The following need to be compiled together with make ../ox/cads/locks/Lock.scala
$(LOCKDIR)/Lock.class: locks/Lock.scala
	fsc -d .. locks/Lock.scala locks/SimpleDelayLock.scala locks/SimpleCondition.scala locks/ArrayQueueLock.scala locks/CLHLock.scala

# The following two need to be compiled together
$(TESTDIR)/Configuration.class: testing/Configuration.scala
	fsc -d .. testing/Configuration.scala testing/ConfigSet.scala

$(CADS)/experiments/specialFunctions.class: experiments/specialFunctions.java
	javac -d .. experiments/specialFunctions.java
# It might be necessary to do fsc -shutdown after compiling specialFunctions and
# before compiling ConfidenceIntervals.scala

$(CADS)/%.class: %.scala
	 fsc -d .. $<
