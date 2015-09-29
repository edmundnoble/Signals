package io.signals

import io.signals.Compiler._
import org.scalatest.{Matchers, WordSpec}

class OutputTests extends WordSpec with Matchers {
  "A struct" should {
    "have correct definitions generated" in {
      val correct =
        """typedef struct type {
          |  Layer layer;
          |} type;""".stripMargin
      Output.generateDefinition("type", StructDef(native = false, Seq(Compiler.StructField("layer", "Layer")))) should be(correct)
    }
    "have correct declarations generated" in {
      val correct =
        """struct type;
          |typedef struct type type;""".stripMargin
      Output.generateForwardTypeDeclaration("type", StructDef(native = false, Seq(Compiler.StructField("layer", "Layer")))) should be(correct)
    }
  }

  "An alias" should {
    "be generated correctly" in {
      val correct =
        """typedef type_from type_to;""".stripMargin
      Output.generateAlias("type_from", "type_to") should be(correct)
    }
  }

  "Signals" should {
    "have correct static declarations" in {
      val correct =
        """static Window *window;
          |
          |static uint64_t sig_test;""".stripMargin
      val testResult = Output.generateStaticDeclarations(
        Compiler.Understood(Map.empty, Map.empty, Seq(Compiler.Signal("sig_test", "uint64_t")), Map.empty)
      )
      testResult should be(correct)
    }
    "have correct handlers" in {
      val correct =
        """void watch_model_sig_test_change_handler(uint64_t new_value) {
        |  sig_test = new_value;
        |  layer_mark_dirty(test_layer);
        |}""".stripMargin
      val program = Compiler.Understood(Map.empty, Map.empty, Seq(Compiler.Signal("sig_test", "uint64_t")), Map("test_layer" -> Compiler.DrawProc("", "")))
      val testResult = Output.generateSignalHandlers(program)
      testResult should be(correct)
    }
  }

  "Layers" should {
    "have correct declarations generated" in {
      val correct = "static Window *window;\nstatic Layer *test_layer;\n"
      val program = Compiler.Understood(Map.empty, Map.empty, Seq.empty, Map("test_layer" -> Compiler.DrawProc("", "")))
      val testResult = Output.generateStaticDeclarations(program)
      testResult should be(correct)
    }
    "have correct draw procs generated" in {
      val correct =
        """static void test_layer_draw_proc(Layer *layer, GContext *ctx) {
          |  test_stuff();
          |}""".stripMargin
      val program = Compiler.Understood(
        Map.empty,
        Map.empty,
        Seq.empty,
        Map("test_layer" -> Compiler.DrawProc("  test_stuff();", "ctx")))
      val testResult = Output.generateDrawProcs(program)
      testResult should be(correct)
    }
    "have their draw procs wired correctly" ignore {
      "No fucks given"
    }
  }

  "Main files" should {
    "be correct" in {
      val correct = """#include "watch_model.h"
                      |
                      |static Window *window;
                      |static Layer *clock_layer;
                      |static GRect sig_rect;
                      |static GPoint sig_pnt;
                      |
                      |void watch_model_sig_rect_change_handler(GRect new_value) {
                      |  sig_rect = new_value;
                      |  layer_mark_dirty(clock_layer);
                      |}
                      |
                      |void watch_model_sig_pnt_change_handler(GPoint new_value) {
                      |  sig_pnt = new_value;
                      |  layer_mark_dirty(clock_layer);
                      |}
                      |
                      |static void clock_layer_draw_proc(Layer *layer, GContext *ctx) {
                      |  graphics_fill_rect(ctx, sig_rect);
                      |}
                      |
                      |static void window_load(Window *window) {
                      |  Layer *const window_layer = window_get_root_layer(window);
                      |  const GRect bounds = layer_get_bounds(window_layer);
                      |  window_set_background_color(window, GColorBlack);
                      |  clock_layer = layer_create(bounds);
                      |  layer_set_update_proc(clock_layer, clock_layer_draw_proc);
                      |  layer_add_child(window_layer, clock_layer);
                      |  watch_model_init();
                      |}
                      |
                      |static void window_unload(Window *window) {
                      |  layer_destroy(clock_layer);
                      |}
                      |
                      |static void init(void) {
                      |  window = window_create();
                      |  window_set_window_handlers(window, (WindowHandlers) {
                      |    .load = window_load,
                      |    .unload = window_unload
                      |  });
                      |  window_stack_push(window, true /* animated */);
                      |}
                      |
                      |static void deinit(void) {
                      |  window_destroy(window);
                      |}
                      |
                      |int main(void) {
                      |  init();
                      |  app_event_loop();
                      |  deinit();
                      |}""".stripMargin

      val typeAliases = Map("uint64_t" -> "qword", "GColor8" -> "GColor", "VoiceUIData" -> "VoiceWindow")
      val structDefinitions = Map("GSize" -> StructDef(false, Seq(StructField("w", "int32_t"), StructField("h", "int32_t"))),
                                  "GPoint" -> StructDef(false, Seq(StructField("x", "int32_t"), StructField("y", "int32_t"))),
                                  "GRect" -> StructDef(false, Seq(StructField("origin", "GPoint"), StructField("size", "GSize"))))
      val signals = Seq(Signal("sig_rect", "GRect"), Signal("sig_pnt", "GPoint"))
      val layers = Map("clock_layer" -> DrawProc("  graphics_fill_rect(ctx, sig_rect);", "ctx"))
      val program = Compiler.Understood(typeAliases, structDefinitions, signals, layers)
      val testResult = Output.generateMainFile(program)
      testResult should be(correct)
    }
  }

  "Interpolators" should {
    "be correct" in {
      val correct =
        """static GSize prv_interpolate_GSize_linear(GSize from, GSize to, const AnimationProgress progress);
          |
          |static int64_t prv_interpolate_int64_t_linear(int64_t from, int64_t to, const AnimationProgress progress) {
          |  return from + ((progress * (to - from)) / ANIMATION_NORMALIZED_MAX);
          |}
          |
          |static GSize prv_interpolate_GSize_linear(GSize from, GSize to, const AnimationProgress progress) {
          |  return (GSize) {
          |    .w = (int32_t) prv_interpolate_int64_t_linear(from.w, to.w, progress),
          |    .h = (int32_t) prv_interpolate_int64_t_linear(from.h, to.h, progress),
          |  };
          |}""".stripMargin
      val program = Understood(
        typeAliases = Map.empty,
        structDefinitions = Map("GSize" -> StructDef(false, Seq(StructField("w", "int32_t"), StructField("h", "int32_t")))),
        signals = Seq.empty, layers = Map.empty)
      val testResult = Output.generateInterpolators(program)
      testResult should be(correct)
    }
  }

}
