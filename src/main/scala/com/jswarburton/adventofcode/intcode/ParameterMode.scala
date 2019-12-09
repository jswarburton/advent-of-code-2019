package com.jswarburton.adventofcode.intcode

sealed trait ParameterMode

case object PositionMode extends ParameterMode

case object ImmediateMode extends ParameterMode
