package com.cbsa.migration.service;

import com.cbsa.migration.model.Control;
import com.cbsa.migration.repository.ControlRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.locks.ReentrantLock;

/**
 * Replaces the COBOL ENQ/DEQ + Named Counter pattern (CRECUST.cbl lines 441-500).
 * Wraps ControlRepository with concurrency control via ReentrantLock,
 * equivalent to CICS ENQ/DEQ on resource CBSACUST.
 */
@Service
public class NamedCounterService {

    private static final Logger logger = LoggerFactory.getLogger(NamedCounterService.class);

    private final ControlRepository controlRepository;
    private final ReentrantLock lock = new ReentrantLock();

    private Long allocatedCustomerNumber;

    public NamedCounterService(ControlRepository controlRepository) {
        this.controlRepository = controlRepository;
    }

    /**
     * Acquires lock and increments the customer number counter.
     * Equivalent to CICS ENQ + Named Counter increment.
     *
     * @param sortCode the branch sort code (used for logging context)
     * @return the next customer number
     * @throws CustomerCreationException with fail code '3' if lock cannot be acquired
     */
    public Long getNextCustomerNumberWithLock(String sortCode) {
        try {
            lock.lock();
        } catch (Exception e) {
            throw new CustomerCreationException("3",
                    "Failed to acquire named counter lock for sort code " + sortCode, e);
        }

        try {
            Long nextNumber = controlRepository.getNextCustomerNumber();
            this.allocatedCustomerNumber = nextNumber;
            logger.info("Allocated customer number {} for sort code {}", nextNumber, sortCode);
            return nextNumber;
        } catch (Exception e) {
            lock.unlock();
            throw new CustomerCreationException("3",
                    "Failed to get next customer number for sort code " + sortCode, e);
        }
    }

    /**
     * Decrements lastCustomerNumber and customerCount in the control record.
     * Called when a subsequent VSAM/PROCTRAN write fails. In COBOL this is
     * done by subtracting 1 from the named counter.
     */
    @Transactional
    public void rollbackCustomerNumber() {
        if (allocatedCustomerNumber == null) {
            logger.warn("No allocated customer number to rollback");
            return;
        }

        try {
            Control control = controlRepository.getControl()
                    .orElseThrow(() -> new IllegalStateException("Control record not found"));
            control.setLastCustomerNumber(control.getLastCustomerNumber() - 1);
            control.setCustomerCount(control.getCustomerCount() - 1);
            controlRepository.save(control);
            logger.info("Rolled back customer number {}. Counter decremented.", allocatedCustomerNumber);
            allocatedCustomerNumber = null;
        } catch (Exception e) {
            logger.error("Failed to rollback customer number {}", allocatedCustomerNumber, e);
        }
    }

    /**
     * Releases the ReentrantLock (equivalent to EXEC CICS DEQ).
     * Should be called in a finally block.
     */
    public void releaseLock() {
        if (lock.isHeldByCurrentThread()) {
            lock.unlock();
            logger.debug("Named counter lock released");
        }
    }
}
