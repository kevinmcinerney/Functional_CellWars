package model

import game.Vertex

import scala.util.Try

/**  Vertex Trait **/
trait V {

  type A <: V

  type B = Int

  val edges: Vector[A]

  def addEdge(other: B): A

  def addEdges(other: Vector[B]): A

  def delEdge(other: B): A

  def delEdges(other: Vector[B]): A

}

/**
  * Created by kevin on 27/03/19.
  */
trait G[A <: V] {

  val vertices: Vector[A]

  def connected[A](other: A): Boolean

  def connect[A](other: A) : A

  def dfs(current: A, vertices: Vector[A], acc: Vector[A] = Vector()): Vector[A]

  def fdfs(vertices: Vector[A]): Vector[Vector[A]]

  def map(current: A, vertices: Vector[A], f: A => A, acc: Vector[A] = Vector()): Vector[A]

}

trait B[A <: V] {

  val g: G[A]

  def up(vertex: A): Try[B[A]]

  def down(vertex: A): Try[B[A]]

  def left(vertex: A): Try[B[A]]

  def right(vertex: A): Try[B[A]]

}
