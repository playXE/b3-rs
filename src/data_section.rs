pub struct DataSection {
    pub data: Vec<u8>,
}

impl DataSection {
    pub fn new(size: usize) -> DataSection {
        DataSection {
            data: vec![0; size],
        }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }

    pub fn size(&self) -> usize {
        self.data.len()
    }
}
