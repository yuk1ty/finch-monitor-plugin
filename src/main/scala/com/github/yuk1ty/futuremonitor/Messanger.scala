package com.github.yuk1ty.futuremonitor

import com.twitter.util.{Future, Var}

/*
 * Copyright 2017 Yuki Toyoda
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

trait Messanger {
  def receive[T](name: String, future: => Future[T]): Future[T]
}

object Messenger extends Messanger {

  private lazy val monitor: Monitor = Monitor

  def receive[T](name: String, future: => Future[T]): Future[T] = {
    val stateCell = Var(sendTask(name))
    future onSuccess { _ =>
      sendSuccess(stateCell)
    } onFailure { err =>
      sendFailure(err, stateCell)
    }
  }

  private def sendTask(name: String): TaskState = {
    val state = TaskState(name = name, status = Start)
    monitor.send(state)
    state
  }

  private def sendSuccess(state: Var[TaskState]): Var[TaskState] = {
    val stateCell = state.map(_.copy(status = Success))
    monitor.send(stateCell.sample())
    stateCell
  }

  private def sendFailure(err: Throwable,
                          state: Var[TaskState]): Var[TaskState] = {
    val stateCell = state.map(_.copy(status = Error(err)))
    monitor.send(stateCell.sample())
    stateCell
  }
}

case class TaskState(name: String, status: TaskStatus)

trait TaskStatus {
  def message: String
}

case object Start extends TaskStatus {
  override def message: String = "Started"
}

case object Success extends TaskStatus {
  override def message: String = "Succeeded"
}

case class Error(err: Throwable) extends TaskStatus {
  override def message: String = s"Unexpected Error: ${err.getMessage}"
}
