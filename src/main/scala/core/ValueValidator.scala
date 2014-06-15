package core

import core.Encoder._
/**
 * Created by smcho on 6/14/14.
 */
object ValueValidator {
  // 0x20 - 0x7E
  def isPrintable(value: Char) : Boolean = {
    if (value.toInt >= 0x20 && value.toInt <= 0x7E) true
    else false
  }
  def isValidReserved(value: Char) : Boolean = {
    if ((value >='a' && value <= 'z') || (value >='0' && value <= '9') || (value == ' ')) true
    else false
  }
  def validateString(value: String) = {
    value.forall(i => isPrintable(i))
  }
  def validateReserved(value: String) = {
    value.forall(i => isValidReserved(i))
  }
  def validateAltitude(valueInput:Option[Float]) = {
    if (valueInput.isEmpty) false
    else {
      val start = altitudeStart + altitudeShift
      val end = altitudeEnd + altitudeShift
      val valueDecoded = valueInput.getOrElse(0.0F) // when value is None
      val value = if (valueDecoded > 0.0F) valueDecoded else -valueDecoded
      if (value >= start && value <= end) true
      else false
    }
  }
  def validateLatitude(valueInput:Option[Float]) = {
    if (valueInput.isEmpty) false
    else {
      val start = latitudeStart + latitudeShift
      val end = latitudeEnd + latitudeShift
      val valueDecoded = valueInput.getOrElse(0.0F) // when value is None
      val value = if (valueDecoded > 0.0F) valueDecoded else -valueDecoded
      if (value >= start && value <= end) true
      else false
    }
  }
  def validateTemperature(valueInput:Option[Float]) = {
    if (valueInput.isEmpty) false
    else {
      val start = tempStart + tempShift
      val end = tempEnd + tempShift

      val value = valueInput.getOrElse(0.0F) // value is not None, so just give it a value
      if (value >= start && value <= end) true
      else false
    }
  }
  def validateDate(valueInput:Option[Int]) = {
    if (valueInput.isEmpty) false
    else {
      val value = valueInput.getOrElse(0) // null makes false
      if (value != 0) {
        val x = if (value > 0) value else -value
        val year = (x % 100000000) / 10000
        val month = (x % 10000) / 100
        val day = x % 100

        if ((year >= -yearLimit && year <= yearLimit) && (month >= 1 && month <= 12) && (day >= 1 && day <= 31))
          true
        else
          false
      }
      else
        false
    }
  }
  def validateTime(valueInput:Option[Int]) = {
    if (valueInput.isEmpty) false
    else {
      val value = valueInput.getOrElse(0) // null makes false
      if (value >= 0) {
        val x = value
        val hour = (x % 100000000) / 1000000
        val minute = (x % 1000000) / 10000
        val second = x % 10000

        if ((hour >= 0 && hour <= 23) && (minute >= 0 && minute <= 59) && (second >= 0 && second <= 5999))
          true
        else
          false
      }
      else
        false
    }
  }
  def validateEnum(valueInput:Option[Int]) = {
    val value = valueInput.getOrElse(-1)
    if (value >= 0 && value <= Encoder.enumLimit) true
    else false
  }

  def validateInt(valueInput:Option[Int]) = {
    if (valueInput.isEmpty) false
    else true // valueInput.getOrElse(-1.0)
  }

  def validateFloat(valueInput:Option[Float]) = {
    if (valueInput.isEmpty) false
    else true // valueInput.getOrElse(-1.0)
  }

  def validateDouble(valueInput:Option[Double]) = {
    if (valueInput.isEmpty) false
    else true // valueInput.getOrElse(-1.0)
  }
}
