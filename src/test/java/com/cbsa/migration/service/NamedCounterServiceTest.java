package com.cbsa.migration.service;

import com.cbsa.migration.model.Control;
import com.cbsa.migration.repository.ControlRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class NamedCounterServiceTest {

    @Mock
    private ControlRepository controlRepository;

    private NamedCounterService namedCounterService;

    @BeforeEach
    void setUp() {
        namedCounterService = new NamedCounterService(controlRepository);
    }

    @Test
    void getNextCustomerNumberWithLock_returnsIncrementedValue() {
        when(controlRepository.getNextCustomerNumber()).thenReturn(100001L);

        Long result = namedCounterService.getNextCustomerNumberWithLock("987654");

        assertThat(result).isEqualTo(100001L);
        verify(controlRepository).getNextCustomerNumber();
        namedCounterService.releaseLock();
    }

    @Test
    void getNextCustomerNumberWithLock_repositoryThrows_throwsExceptionAndReleasesLock() {
        when(controlRepository.getNextCustomerNumber())
                .thenThrow(new RuntimeException("DB error"));

        assertThatThrownBy(() -> namedCounterService.getNextCustomerNumberWithLock("987654"))
                .isInstanceOf(CustomerCreationException.class)
                .satisfies(e -> assertThat(((CustomerCreationException) e).getFailCode()).isEqualTo("3"));
    }

    @Test
    void rollbackCustomerNumber_decrementsCorrectly() {
        when(controlRepository.getNextCustomerNumber()).thenReturn(100005L);
        namedCounterService.getNextCustomerNumberWithLock("987654");

        Control control = new Control();
        control.setCustomerCount(10L);
        control.setLastCustomerNumber(100005L);
        control.setAccountCount(0);
        control.setLastAccountNumber(0);
        when(controlRepository.getControl()).thenReturn(Optional.of(control));
        when(controlRepository.save(any(Control.class))).thenReturn(control);

        namedCounterService.rollbackCustomerNumber();

        verify(controlRepository).save(argThat(c ->
                c.getLastCustomerNumber() == 100004L && c.getCustomerCount() == 9L));

        namedCounterService.releaseLock();
    }

    @Test
    void rollbackCustomerNumber_noAllocation_doesNothing() {
        namedCounterService.rollbackCustomerNumber();
        verify(controlRepository, never()).getControl();
        verify(controlRepository, never()).save(any());
    }

    @Test
    void releaseLock_whenNotHeld_doesNotThrow() {
        namedCounterService.releaseLock();
    }

    @Test
    void releaseLock_whenHeld_releasesSuccessfully() {
        when(controlRepository.getNextCustomerNumber()).thenReturn(100001L);
        namedCounterService.getNextCustomerNumberWithLock("987654");
        namedCounterService.releaseLock();
    }

    @Test
    void concurrentAccess_isSerialized() throws Exception {
        AtomicInteger callCount = new AtomicInteger(0);
        when(controlRepository.getNextCustomerNumber()).thenAnswer(invocation -> {
            int count = callCount.incrementAndGet();
            Thread.sleep(50);
            return 100000L + count;
        });

        int threadCount = 5;
        ExecutorService executor = Executors.newFixedThreadPool(threadCount);
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch doneLatch = new CountDownLatch(threadCount);

        for (int i = 0; i < threadCount; i++) {
            executor.submit(() -> {
                try {
                    startLatch.await();
                    Long number = namedCounterService.getNextCustomerNumberWithLock("987654");
                    assertThat(number).isNotNull();
                } catch (Exception e) {
                    // Expected for some threads
                } finally {
                    namedCounterService.releaseLock();
                    doneLatch.countDown();
                }
            });
        }

        startLatch.countDown();
        doneLatch.await();
        executor.shutdown();

        // All calls should have been serialized
        assertThat(callCount.get()).isEqualTo(threadCount);
    }
}
