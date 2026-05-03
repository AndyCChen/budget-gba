extends Node

var budget_gba: BudgetGba;

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	budget_gba = BudgetGba.new();

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	budget_gba.on_update(delta);
