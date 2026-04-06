use crate::codegen::IRSet;

pub trait OptimizationPass: 'static {
    fn name(&self) -> &'static str;
    fn optimize(&mut self, ir: &mut IRSet);
}

#[derive(Default)]
pub struct Optimizer {
    passes: Vec<Box<dyn OptimizationPass>>,
}

impl Optimizer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn default_pipeline() -> Self {
        Self::new()
    }

    pub fn is_empty(&self) -> bool {
        self.passes.is_empty()
    }

    pub fn add_pass<P>(&mut self, pass: P) -> &mut Self
    where
        P: OptimizationPass,
    {
        self.passes.push(Box::new(pass));
        self
    }

    pub fn with_pass<P>(mut self, pass: P) -> Self
    where
        P: OptimizationPass,
    {
        self.add_pass(pass);
        self
    }

    pub fn optimize(&mut self, ir: &mut IRSet) {
        for pass in &mut self.passes {
            let _ = pass.name();
            pass.optimize(ir);
        }
    }
}
