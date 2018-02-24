package com.github.yuk1ty.futuremonitor

import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConverters._

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

trait Monitor {

  def send(state: TaskState): Unit

  def show(): List[(String, TaskStatus)]
}

object Monitor extends Monitor {

  private[this] lazy val monitor = new ConcurrentHashMap[String, TaskStatus]()

  override def send(state: TaskState): Unit = {
    state.status match {
      case start @ Start     => monitor.putIfAbsent(state.name, start)
      case success @ Success => monitor.put(state.name, success)
      case error @ Error(_)  => monitor.put(state.name, error)
    }
  }

  override def show(): List[(String, TaskStatus)] = monitor.asScala.toList
}
