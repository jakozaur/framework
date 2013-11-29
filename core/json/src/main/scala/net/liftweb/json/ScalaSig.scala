/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package json

import scala.reflect.runtime.{universe => ru}

private[json] object ScalaSigReader {
  def readConstructor(argName: String, clazz: Class[_], typeArgIndex: Int, argNames: List[String]): Class[_] = {
    this.synchronized {
      val cl = findClass(clazz)
      val cstr = findConstructor(cl, argNames).getOrElse(Meta.fail("Can't find constructor for " + clazz))
      findArgType(cstr, argNames.indexOf(argName), typeArgIndex)
    }
  }

  def readField(name: String, clazz: Class[_], typeArgIndex: Int): Class[_] = {
    this.synchronized {
      def read(current: Class[_]): ru.MethodSymbol = {
        if (current == null)
          Meta.fail("Can't find field " + name + " from " + clazz)
        else
          findField(findClass(current), name).getOrElse(read(current.getSuperclass))
      }
      findArgTypeForField(read(clazz), typeArgIndex)
    }
  }

  private def findClass(clazz: Class[_]): ru.ClassSymbol = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    mirror.classSymbol(clazz)
  }

  private def findConstructor(c: ru.ClassSymbol, argNames: List[String]): Option[ru.MethodSymbol] = {
    val constructors = c.toType.members.filter(_.isMethod).map(_.asMethod).filter(_.isConstructor)
    constructors.find(
      _.paramss.headOption.map(_.map(_.name.encoded)).getOrElse(List()) == argNames)
  }

  private def findField(c: ru.ClassSymbol, name: String): Option[ru.MethodSymbol] = {
    val methods = c.toType.members.filter(_.isMethod).map(_.asMethod)
    methods.find(_.name.encoded == name)
  }

  private def findArgType(s: ru.MethodSymbol, argIdx: Int, typeArgIndex: Int): Class[_] = {
    def findPrimitive(t: ru.Type): ru.Symbol = t match {
      case ru.TypeRef(ru.ThisType(_), symbol, _) if isPrimitive(symbol) => symbol
      case ru.TypeRef(_, _, ru.TypeRef(ru.ThisType(_), symbol, _) :: xs) => symbol
      case ru.TypeRef(_, symbol, Nil) => symbol
      case ru.TypeRef(_, _, args) if typeArgIndex >= args.length => findPrimitive(args(0))
      case ru.TypeRef(_, _, args) =>
        args(typeArgIndex) match {
          case ref @ ru.TypeRef(_, _, _) => findPrimitive(ref)
          case x => Meta.fail("Unexpected type info " + x)
        }
      case x => Meta.fail("Unexpected type info " + x)
    }
    toClass(findPrimitive(s.paramss.head(argIdx).typeSignature))
  }

  private def findArgTypeForField(s: ru.MethodSymbol, typeArgIdx: Int): Class[_] = {
    val returnType = s.returnType
    if (!returnType.isInstanceOf[ru.TypeRef]) {
      Meta.fail(s"Unexpected returnType type of $s")
    }

    val t = returnType.asInstanceOf[ru.TypeRef].args(typeArgIdx)

    val symbol = t.typeSymbol
    if (symbol == ru.NoSymbol) {
      Meta.fail(s"Unexpected type info $t")
    }
    toClass(symbol)
  }

  private def toClass(s: ru.Symbol) = s.fullName match {
    case "scala.Short"   => classOf[Short]
    case "scala.Int"     => classOf[Int]
    case "scala.Long"    => classOf[Long]
    case "scala.Boolean" => classOf[Boolean]
    case "scala.Float"   => classOf[Float]
    case "scala.Double"  => classOf[Double]
    case _               => classOf[AnyRef]
  }

  private def isPrimitive(s: ru.Symbol) = toClass(s) != classOf[AnyRef]
}
